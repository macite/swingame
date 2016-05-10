//
//  test_network.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 27/11/2014.
//  Copyright (c) 2014 Andrew Cain. All rights reserved.
//

#include "test_network.h"

#include <iostream>
#include <stdio.h>

#include "sgInterfaces.h"

using namespace std;

extern sg_interface * _sg_functions;

void test_web()
{
    cout << "Testing SwinGame web functions" << endl;

    sg_http_request get_request = { HTTP_GET, "http://google.com", 80, NULL };
    sg_http_response get_response;

    get_response = _sg_functions->web.http_request(get_request);

    printf("GET RESPONSE = %hu\n%s\n\n", get_response.status, get_response.data);

    sg_http_request post_request = { HTTP_POST, "http://127.0.0.1:3000/api/auth", 3000, "{\"username\":\"acain\",\"password\":\"password\",\"remember\":true}" };
    sg_http_response post_response;

    post_response = _sg_functions->web.http_request(post_request);

    printf("POST RESPONSE = %hu\n%s\n\n", post_response.status, post_response.data);

    sg_http_request put_request = { HTTP_PUT, "http://127.0.0.1:3000/api/auth/zExXS_nc3x9wEb96aWxq", 3000, "{\"username\":\"acain\"}" };
    sg_http_response put_response;

    put_response = _sg_functions->web.http_request(put_request);

    printf("PUT RESPONSE = %hu\n%s\n\n", put_response.status, put_response.data);

    sg_http_request delete_request = { HTTP_DELETE, "http://127.0.0.1:3000/api/auth/zExXS_nc3x9wEb96aWxq", 3000, "{\"username\":\"acain\"}" };
    sg_http_response delete_response;

    delete_response = _sg_functions->web.http_request(delete_request);

    printf("DELETE RESPONSE = %hu\n%s\n\n", delete_response.status, delete_response.data);
}

void test_network()
{
    test_web();

    cout << "Opening socket at localhost:49000" << endl;

    sg_network_connection svr, client_to_svr, svr_to_client;
    svr = _sg_functions->network.open_tcp_connection(nullptr, 49000);

    client_to_svr = _sg_functions->network.open_tcp_connection("127.0.0.1", 49000);

    svr_to_client = _sg_functions->network.accept_new_connection(&svr);

    char buffer[1024] = "hello";

    cout << "Sending 'hello'" << endl;

    cout << "send response: " << _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);
    cout << "read response: " << _sg_functions->network.read_bytes(&client_to_svr, buffer, 1024);

    cout << "Read " << buffer << endl;

    int i;
    cout << "closing (enter number to continue)" << endl;
    _sg_functions->network.close_connection(&client_to_svr);
    cin >> i;

    cout << "sending again..." << endl;
    cout << "send response: " << _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);

    cout << "enter a number: ";
    cin >> i;
    cout << "sending again..." << endl;
    cout << "send response: " << _sg_functions->network.send_bytes(&svr_to_client, buffer, 1024);
}
