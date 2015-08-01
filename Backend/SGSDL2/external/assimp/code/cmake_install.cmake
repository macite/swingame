# Install script for directory: /Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Debug/libassimpd.3.1.1.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Debug/libassimpd.3.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Debug/libassimpd.dylib"
      )
    foreach(file
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimpd.3.1.1.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimpd.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimpd.dylib"
        )
      if(EXISTS "${file}" AND
         NOT IS_SYMLINK "${file}")
        execute_process(COMMAND "/usr/bin/install_name_tool"
          -id "/usr/local//libassimpd.3.dylib"
          "${file}")
        if(CMAKE_INSTALL_DO_STRIP)
          execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "${file}")
        endif()
      endif()
    endforeach()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Release/libassimp.3.1.1.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Release/libassimp.3.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Release/libassimp.dylib"
      )
    foreach(file
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.1.1.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.dylib"
        )
      if(EXISTS "${file}" AND
         NOT IS_SYMLINK "${file}")
        execute_process(COMMAND "/usr/bin/install_name_tool"
          -id "/usr/local//libassimp.3.dylib"
          "${file}")
        if(CMAKE_INSTALL_DO_STRIP)
          execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "${file}")
        endif()
      endif()
    endforeach()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Mm][Ii][Nn][Ss][Ii][Zz][Ee][Rr][Ee][Ll])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/MinSizeRel/libassimp.3.1.1.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/MinSizeRel/libassimp.3.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/MinSizeRel/libassimp.dylib"
      )
    foreach(file
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.1.1.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.dylib"
        )
      if(EXISTS "${file}" AND
         NOT IS_SYMLINK "${file}")
        execute_process(COMMAND "/usr/bin/install_name_tool"
          -id "/usr/local//libassimp.3.dylib"
          "${file}")
        if(CMAKE_INSTALL_DO_STRIP)
          execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "${file}")
        endif()
      endif()
    endforeach()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/RelWithDebInfo/libassimp.3.1.1.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/RelWithDebInfo/libassimp.3.dylib"
      "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/RelWithDebInfo/libassimp.dylib"
      )
    foreach(file
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.1.1.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libassimp.dylib"
        )
      if(EXISTS "${file}" AND
         NOT IS_SYMLINK "${file}")
        execute_process(COMMAND "/usr/bin/install_name_tool"
          -id "/usr/local//libassimp.3.dylib"
          "${file}")
        if(CMAKE_INSTALL_DO_STRIP)
          execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "${file}")
        endif()
      endif()
    endforeach()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "assimp-dev")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/assimp" TYPE FILE FILES
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/anim.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/ai_assert.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/camera.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/color4.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/color4.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/config.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/defs.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/cfileio.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/light.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/material.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/material.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/matrix3x3.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/matrix3x3.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/matrix4x4.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/matrix4x4.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/mesh.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/postprocess.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/quaternion.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/quaternion.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/scene.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/metadata.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/texture.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/types.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/vector2.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/vector2.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/vector3.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/vector3.inl"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/version.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/cimport.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/importerdesc.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Importer.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/DefaultLogger.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/ProgressHandler.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/IOStream.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/IOSystem.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Logger.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/LogStream.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/NullLogger.hpp"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/cexport.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Exporter.hpp"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "assimp-dev")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/assimp/Compiler" TYPE FILE FILES
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Compiler/pushpack1.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Compiler/poppack1.h"
    "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/code/../include/assimp/Compiler/pstdint.h"
    )
endif()

