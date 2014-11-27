//
//  test_network.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 27/11/2014.
//  Copyright (c) 2014 Andrew Cain. All rights reserved.
//

#include "test_network.h"

#include <iostream>

#include "sgInterfaces.h"

using namespace std;

extern sg_interface * _sg_functions;

void test_network()
{
    sg_network_connection svr, client_to_svr, svr_to_client;
    svr = _sg_functions->network.open_tcp_connection(nullptr, 49000);
    
    client_to_svr = _sg_functions->network.open_tcp_connection("127.0.0.1", 49000);
    
    svr_to_client = _sg_functions->network.accept_new_connection(&svr);
    
    char buffer[1024] = "hello";
    
    cout << "test" << endl;
    
    _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);
    _sg_functions->network.read_bytes(&client_to_svr, buffer, 1024);

    int i;
    cout << "closing" << endl;
    _sg_functions->network.close_connection(&client_to_svr);
    cin >> i;
    
    cout << "sending again..." << endl;
    _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);

    cout << "enter a number: ";
    cin >> i;
    cout << "sending again..." << endl;
    _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);
}
