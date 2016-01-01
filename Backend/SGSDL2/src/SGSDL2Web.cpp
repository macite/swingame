//
//  SGSDL2Web.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 1/05/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Web.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <curl/curl.h>

typedef struct memory_struct {
    char *memory;
    size_t size;
} memory_struct;


static size_t write_memory_callback(void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;
    memory_struct *mem = (memory_struct *)userp;
    
    mem->memory = (char *)realloc(mem->memory, mem->size + realsize + 1);
    if(mem->memory == NULL) {
        /* out of memory! */
        printf("not enough memory (realloc returned NULL)\n");
        return 0;
    }
    
    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;
    
    return realsize;
}

void sgsdk2_init_web()
{
    curl_global_init(CURL_GLOBAL_ALL);
}

void sgsdl2_finalise_web()
{
    curl_global_cleanup();
}

void sgsdl2_http_post(const char *host, unsigned short port, const char *body)
{
    CURL *curl_handle;
    CURLcode res;
    
    memory_struct chunk;
    
    chunk.memory = (char *)malloc(1);  /* will be grown as needed by the realloc above */
    chunk.size = 0;    /* no data at this point */
    
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
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&chunk);
    
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
        /*
         * Now, our chunk.memory points to a memory block that is chunk.size
         * bytes big and contains the remote file.
         *
         * Do something nice with it!
         */
        
        printf("%lu bytes retrieved\n", (long)chunk.size);
        printf("%s", chunk.memory);
    }
    
    /* cleanup curl stuff */
    curl_easy_cleanup(curl_handle);
    
    free(chunk.memory);
}

void sgsdl2_http_get(const char *host, unsigned short port)
{
    CURL *curl_handle;
    CURLcode res;
    
    memory_struct chunk;
    
    chunk.memory = (char *)malloc(1);  /* will be grown as needed by the realloc above */
    chunk.size = 0;    /* no data at this point */
    
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
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&chunk);
    
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
        /*
         * Now, our chunk.memory points to a memory block that is chunk.size
         * bytes big and contains the remote file.
         *
         * Do something nice with it!
         */
        
        printf("%lu bytes retrieved\n", (long)chunk.size);
        printf("%s", chunk.memory);
    }
    
    /* cleanup curl stuff */
    curl_easy_cleanup(curl_handle);
    
    free(chunk.memory);
}


void sgsdl2_load_web_fns(sg_interface *functions)
{
    curl_global_init(CURL_GLOBAL_ALL);
    
    functions->web.http_get = &sgsdl2_http_get;
    functions->web.http_post = &sgsdl2_http_post;
}
