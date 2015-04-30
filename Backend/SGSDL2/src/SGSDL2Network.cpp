//
//  SGSDL2Network.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 16/11/2014.
//  Copyright (c) 2014 Andrew Cain. All rights reserved.
//

#include "SGSDL2Network.h"

#include <stdio.h>

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_net.h>
#else
#include <SDL.h>
#include <SDL_net.h>
#endif

#include <string.h>
#include <stdlib.h>

// This set keeps track of all of the sockets to see if there is activity
SDLNet_SocketSet _sockets; // allocate on setup of functions.

sg_network_connection sgsdl2_open_udp_connection(unsigned short port)
{
    UDPsocket svr;
    
    sg_network_connection result;
    result.kind = SGCK_UNKNOWN;
    result._socket = NULL;
    
    svr = SDLNet_UDP_Open(port);
    
    if (svr)
    {
        result.kind = SGCK_UDP;
        result._socket = svr;
        SDLNet_UDP_AddSocket(_sockets, svr);
    }
    else
    {
        result.kind = SGCK_UNKNOWN;
        result._socket = NULL;
    }
    
    return result;
}

sg_network_connection sgsdl2_open_tcp_connection(const char *host, unsigned short port)
{
    IPaddress addr;
    TCPsocket client;
    
    sg_network_connection result;
    result.kind = SGCK_UNKNOWN;
    result._socket = NULL;
    
    if (SDLNet_ResolveHost(&addr, host, port) < 0)
        return result;
    
    client = SDLNet_TCP_Open(&addr);
    
    if (client)
    {
        result.kind = SGCK_TCP;
        result._socket = client;
        if (host)
            SDLNet_TCP_AddSocket(_sockets, client);
    }
    else
    {
        result.kind = SGCK_UNKNOWN;
        result._socket = NULL;
    }
    
    return result;
}

int sgsdl2_send_bytes(sg_network_connection *con, char *buffer, int size)
{
//    printf("Sending %d\n", size);
    int sent = 0;
    if ((TCPsocket)con->_socket)
    {
        //printf("here -- %p\n", (TCPsocket)con->_socket);
        sent = SDLNet_TCP_Send((TCPsocket)con->_socket, buffer, size);
    }
//    printf("Sent %d\n", sent);
    return sent;
}

int sgsdl2_send_udp_message(sg_network_connection *con, const char *host, unsigned short port, const char *buffer, int size)
{
    UDPpacket packet;
    SDLNet_ResolveHost(&packet.address, host, port);
  
    packet.len = size;
    packet.data = (Uint8*)buffer;
    return SDLNet_UDP_Send((UDPsocket)con->_socket, -1, &packet);
}

void sgsdl2_read_udp_message(sg_network_connection *con, unsigned int *host, unsigned short *port, char *buffer, unsigned int *size)
{
//    printf("Reading up to %d bytes\n", *size);
    
    UDPpacket *packet;
    packet = SDLNet_AllocPacket((int)*size);
    
    *size = 0;
    *host = 0;
    if ( SDLNet_UDP_Recv((UDPsocket)con->_socket, packet) > 0 )
    {
//        printf("Read %d bytes\n", packet->len);
        
        *host = SDLNet_Read32(&packet->address.host);
        *port = SDLNet_Read16(&packet->address.port);
        
        if ( packet->len > 0 )
        {
            unsigned int buf_sz = sizeof(char) * (unsigned int)packet->len;
            memcpy(buffer, packet->data, buf_sz);
            *size = buf_sz;
        }
    }
    SDLNet_FreePacket(packet);
}

int sgsdl2_read_bytes(sg_network_connection *con, char *buffer, int size)
{
    return SDLNet_TCP_Recv((TCPsocket)con->_socket, buffer, size);
}

void sgsdl2_close_connection(sg_network_connection *con)
{
    if ( con->kind == SGCK_TCP )
    {
        SDLNet_TCP_DelSocket(_sockets, (TCPsocket)con->_socket);
        SDLNet_TCP_Close((TCPsocket)con->_socket);
    }
    else
    {
        SDLNet_UDP_DelSocket(_sockets, (UDPsocket)con->_socket);
        SDLNet_UDP_Close((UDPsocket)con->_socket);
    }
    
    con->kind = SGCK_UNKNOWN;
}

unsigned int sgsdl2_network_address(sg_network_connection *con)
{
    IPaddress *remote;
    if (con->kind == SGCK_TCP)
        remote = SDLNet_TCP_GetPeerAddress((TCPsocket)con->_socket);
    else
        remote = SDLNet_UDP_GetPeerAddress((UDPsocket)con->_socket, -1);
    return SDLNet_Read32(&remote->host);
}

unsigned int sgsdl2_get_network_port(sg_network_connection *con)
{
    IPaddress *remote;
    if (con->kind == SGCK_TCP)
        remote = SDLNet_TCP_GetPeerAddress((TCPsocket)con->_socket);
    else
        remote = SDLNet_UDP_GetPeerAddress((UDPsocket)con->_socket, -1);
    return SDLNet_Read16(&remote->port);
}

sg_network_connection sgsdl2_accept_connection(sg_network_connection *con)
{
    sg_network_connection result;
    result._socket = NULL;
    result.kind = SGCK_UNKNOWN;
    
    TCPsocket client;
    if ((client = SDLNet_TCP_Accept((TCPsocket)con->_socket)) != NULL)
    {
//        printf("Adding client %p\n", client);
        SDLNet_TCP_AddSocket(_sockets, client);
        result._socket = client;
        result.kind = SGCK_TCP;
    }
    return result;
}

unsigned int sgsdl2_network_has_data()
{
    if (SDLNet_CheckSockets(_sockets, 0) > 0) return 1;
    else return 0;
}

unsigned int sgsdl2_connection_has_data(sg_network_connection *con)
{
    int got = SDLNet_SocketReady(con->_socket);

    //    printf("Checking %p %d\n", con->_socket, got);
    if (got > 0)
        return 1;
    else
        return 0;
}

void sgsdl2_load_network_fns(sg_interface *functions)
{
    SDLNet_Init();
    _sockets = SDLNet_AllocSocketSet(1024);
    if(!_sockets)
    {
        printf("Error allocating network resources\n");
        exit(1);
    }
    
    functions->network.open_tcp_connection = &sgsdl2_open_tcp_connection;
    functions->network.open_udp_connection = &sgsdl2_open_udp_connection;
    functions->network.read_bytes = &sgsdl2_read_bytes;
    functions->network.send_bytes = &sgsdl2_send_bytes;
    functions->network.close_connection = &sgsdl2_close_connection;
    functions->network.network_address = &sgsdl2_network_address;
    functions->network.accept_new_connection = &sgsdl2_accept_connection;
    functions->network.network_has_data = &sgsdl2_network_has_data;
    functions->network.connection_has_data = &sgsdl2_connection_has_data;
    functions->network.network_port = &sgsdl2_get_network_port;
    functions->network.send_udp_message = &sgsdl2_send_udp_message;
    functions->network.read_udp_message = &sgsdl2_read_udp_message;
    
//    printf("Network port C: %p = %p\n", functions->network.network_port, &sgsdl2_get_network_port);
}
