//
//  SGSDL2Network.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 16/11/2014.
//  Copyright (c) 2014 Andrew Cain. All rights reserved.
//

#include "SGSDL2Network.h"

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_net.h>
#else
#include <SDL.h>
#include <SDL_net.h>
#endif

sg_network_connection sgsdl2_open_tcp_connection(const char *host, unsigned short port)
{
    IPaddress addr;
    
    sg_network_connection result;
    result.kind = SGCK_UNKNOWN;
    result._socket = NULL;
    
    if (SDLNet_ResolveHost(&addr, host, port) < 0)
        return result;
    
    result.kind = SGCK_TCP;
    result._socket = SDLNet_TCP_Open(&addr);
    return result;
}

int sgsdl2_send_bytes(sg_network_connection *con, char *buffer, int size)
{
    return SDLNet_TCP_Send((TCPsocket)con->_socket, buffer, size);
}

int sgsdl2_read_bytes(sg_network_connection *con, char *buffer, int size)
{
    return SDLNet_TCP_Recv((TCPsocket)con->_socket, buffer, size);
}

void sgsdl2_close_connection(sg_network_connection *con)
{
    SDLNet_TCP_Close((TCPsocket)con->_socket);
    con->kind = SGCK_UNKNOWN;
}

unsigned int sgsdl2_network_address(sg_network_connection *con)
{
    IPaddress *remote;
    remote = SDLNet_TCP_GetPeerAddress((TCPsocket)con->_socket);
    return SDLNet_Read32(&remote->host);
}

sg_network_connection sgsdl2_accept_connection(sg_network_connection *con)
{
    sg_network_connection result;
    result._socket = NULL;
    result.kind = SGCK_UNKNOWN;
    
    TCPsocket client;
    if ((client = SDLNet_TCP_Accept((TCPsocket)con->_socket)) != NULL)
    {
        result._socket = client;
        result.kind = SGCK_TCP;
    }
    return result;
}

void sgsdl2_load_network_fns(sg_interface *functions)
{
    functions->network.open_tcp_connection = &sgsdl2_open_tcp_connection;
    functions->network.read_bytes = &sgsdl2_read_bytes;
    functions->network.send_bytes = &sgsdl2_send_bytes;
    functions->network.close_connection = &sgsdl2_close_connection;
    functions->network.network_address = &sgsdl2_network_address;
    functions->network.accept_new_connection = &sgsdl2_accept_connection;
}
