# Install script for directory: /Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/tools/assimp_cmd

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

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "assimp-bin")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/bin/Debug/assimpd")
    if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd" AND
       NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd")
      execute_process(COMMAND "/usr/bin/install_name_tool"
        -change "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Debug/libassimpd.3.dylib" "/usr/local//libassimpd.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimpd")
      endif()
    endif()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/bin/Release/assimp")
    if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp" AND
       NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND "/usr/bin/install_name_tool"
        -change "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/Release/libassimp.3.dylib" "/usr/local//libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      endif()
    endif()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Mm][Ii][Nn][Ss][Ii][Zz][Ee][Rr][Ee][Ll])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/bin/MinSizeRel/assimp")
    if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp" AND
       NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND "/usr/bin/install_name_tool"
        -change "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/MinSizeRel/libassimp.3.dylib" "/usr/local//libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      endif()
    endif()
  elseif("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/bin/RelWithDebInfo/assimp")
    if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp" AND
       NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND "/usr/bin/install_name_tool"
        -change "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib/RelWithDebInfo/libassimp.3.dylib" "/usr/local//libassimp.3.dylib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      execute_process(COMMAND /usr/bin/install_name_tool
        -delete_rpath "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/external/assimp/lib"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/assimp")
      endif()
    endif()
  endif()
endif()

