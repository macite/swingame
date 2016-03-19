//
//  SGSDL2Web.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 1/05/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Core.h"
#include "SGSDL2Web.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <curl/curl.h>

static size_t write_memory_callback(void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;
    sg_http_response *mem = (sg_http_response *)userp;

    mem->data = (char *)realloc(mem->data, mem->size + realsize + 1);
    if(mem->data == NULL) {
        /* out of memory! */
        printf("not enough memory (realloc returned NULL)\n");
        return 0;
    }

    memcpy(&(mem->data[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->data[mem->size] = 0;

    return realsize;
}

typedef struct request_stream
{
    const char *body;
    unsigned long at;
} request_stream;

/* NOTE: check note regarding reading if this does not work on Windows with libcurl as a
 DLL -- you MUST also provide a read callback with CURLOPT_READFUNCTION.
 Failing to do so will give you a crash since a DLL may not use the
 variable's memory when passed in to it from an app like this. */
static size_t read_request_body(void *ptr, size_t size, size_t nmemb, void *stream)
{
    // not actually a
    request_stream *request = (request_stream *)stream;

    unsigned long str_len = strlen(request->body);

    if (str_len - request->at > 0)
    {
        size_t len = str_len - request->at;
        size_t max_read = nmemb * size;
        size_t to_read;

        if (max_read > len) to_read = len;
        else to_read = max_read;

        strncpy((char*)ptr, request->body + request->at, sizeof(char) * to_read);
        request->at += to_read;
        return to_read;
    }

    return 0;
}

void sgsdk2_init_web()
{
    curl_global_init(CURL_GLOBAL_ALL);
}

void sgsdl2_finalise_web()
{
    curl_global_cleanup();
}

//TODO: fix code duplication here...

sg_http_response sgsdl2_http_post(const char *host, unsigned short port, const char *body)
{
    sg_http_response result = { 500, 0, NULL };
    CURL *curl_handle;
    CURLcode res;

    // init the curl session
    curl_handle = curl_easy_init();

    // specify URL to get
    curl_easy_setopt(curl_handle, CURLOPT_URL, host);

    // set port
    curl_easy_setopt(curl_handle, CURLOPT_PORT, port);

    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);

    curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);

    curl_easy_setopt(curl_handle, CURLOPT_SSL_VERIFYPEER, 0L);

    // header list
    struct curl_slist *list = NULL;

    list = curl_slist_append(list, "Content-Type: application/json;charset=UTF8");
    list = curl_slist_append(list, "Accept: application/json, text/plain, */*");
    curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, list);

    // send all data to this function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);

    // pass in the result as the location to write to
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&result);

    // some servers don't like requests that are made without a user-agent field, so we provide one
    curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "libcurl-agent/1.0");

    // get it!
    res = curl_easy_perform(curl_handle);

    // free headers
    curl_slist_free_all(list);

    // check for errors
    if(res != CURLE_OK)
    {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    }
    else
    {
        long status;
        curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &status);
        result.status = static_cast<unsigned short>(status);
    }

    /* cleanup curl stuff */
    curl_easy_cleanup(curl_handle);

    return result;
}

sg_http_response sgsdl2_http_get(const char *host, unsigned short port)
{
    sg_http_response result = { 500, 0, NULL };
    CURL *curl_handle;
    CURLcode res;

    // init the curl session
    curl_handle = curl_easy_init();

    // specify URL to get
    curl_easy_setopt(curl_handle, CURLOPT_URL, host);

    // set port
    curl_easy_setopt(curl_handle, CURLOPT_PORT, port);

    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);

    // send all data to this function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);

    // we pass our 'chunk' struct to the callback function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&result);

    // some servers don't like requests that are made without a user-agent field, so we provide one
    curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "libcurl-agent/1.0");

    // get it!
    res = curl_easy_perform(curl_handle);

    // check for errors
    if(res != CURLE_OK)
    {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    }
    else
    {
        long status;
        curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &status);
        result.status = static_cast<unsigned short>(status);
    }

    /* cleanup curl stuff */
    curl_easy_cleanup(curl_handle);

    return result;
}

sg_http_response sgsdl2_http_put(const char *host, unsigned short port, const char *body)
{
    sg_http_response result = { 500, 0, NULL };
    CURL *curl_handle;
    CURLcode res;

    // init the curl session
    curl_handle = curl_easy_init();

    // specify URL to get
    curl_easy_setopt(curl_handle, CURLOPT_URL, host);

    // set port
    curl_easy_setopt(curl_handle, CURLOPT_PORT, port);

    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);

    // header list
    struct curl_slist *list = NULL;
    list = curl_slist_append(list, "Content-Type: application/json;charset=UTF8");
    list = curl_slist_append(list, "Accept: application/json, text/plain, */*");
    curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, list);

    // send all data to this function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);

    // we pass our 'chunk' struct to the callback function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&result);

    // some servers don't like requests that are made without a user-agent field, so we provide one
    curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "libcurl-agent/1.0");

    /* we want to use our own read function */
    curl_easy_setopt(curl_handle, CURLOPT_READFUNCTION, read_request_body);

    /* enable uploading */
    curl_easy_setopt(curl_handle, CURLOPT_UPLOAD, 1L);

    request_stream data = { body, 0 };

    /* now specify which pointer to pass to our callback */
    curl_easy_setopt(curl_handle, CURLOPT_READDATA, &data);

    /* Set the size of the file to upload */
    curl_easy_setopt(curl_handle, CURLOPT_INFILESIZE, (curl_off_t)strlen(data.body));

    /* Now run off and do what you've been told! */
    res = curl_easy_perform(curl_handle);

    // check for errors
    if(res != CURLE_OK)
    {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    }
    else
    {
        long status;
        curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &status);
        result.status = static_cast<unsigned short>(status);
    }

    // free headers
    curl_slist_free_all(list);

    curl_easy_cleanup(curl_handle);

    return result;
}

sg_http_response sgsdl2_http_delete(const char *host, unsigned short port, const char *body)
{
    sg_http_response result = { 500, 0, NULL };
    CURL *curl_handle;
    CURLcode res;

    // init the curl session
    curl_handle = curl_easy_init();

    // specify URL to get
    curl_easy_setopt(curl_handle, CURLOPT_URL, host);

    // set port
    curl_easy_setopt(curl_handle, CURLOPT_PORT, port);

    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);

    curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);

    // header list
    struct curl_slist *list = NULL;
    list = curl_slist_append(list, "Content-Type: application/json;charset=UTF8");
    list = curl_slist_append(list, "Accept: application/json, text/plain, */*");
    curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, list);

    // send all data to this function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);

    // we pass our 'chunk' struct to the callback function
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&result);

    // some servers don't like requests that are made without a user-agent field, so we provide one
    curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "libcurl-agent/1.0");

    curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, "DELETE");

    /* Now run off and do what you've been told! */
    res = curl_easy_perform(curl_handle);

    // check for errors
    if(res != CURLE_OK)
    {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    }
    else
    {
        long status;
        curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &status);
        result.status = static_cast<unsigned short>(status);
    }

    // free headers
    curl_slist_free_all(list);

    curl_easy_cleanup(curl_handle);

    return result;
}

sg_http_response sgsdl2_http_request(sg_http_request request)
{
    internal_sgsdl2_init();
    
    switch (request.request_type) {
        case HTTP_GET:
            return sgsdl2_http_get(request.url, request.port);
        case HTTP_POST:
            return sgsdl2_http_post(request.url, request.port, request.body);
        case HTTP_PUT:
            return sgsdl2_http_put(request.url, request.port, request.body);
        case HTTP_DELETE:
            return sgsdl2_http_delete(request.url, request.port, request.body);
        default:
            return { 500, 0, NULL };
    }
}

void sgsdl2_free_response(sg_http_response *response)
{
    if ( response && response->size > 0 )
    {
        free(response->data);
        response->data = NULL;
        response->size = 0;
        response->status = 0;
    }
}


void sgsdl2_load_web_fns(sg_interface *functions)
{
    curl_global_init(CURL_GLOBAL_ALL);

    functions->web.http_request = & sgsdl2_http_request;
    functions->web.free_response = & sgsdl2_free_response;
}
