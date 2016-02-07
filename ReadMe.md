# SwinGame 

SwinGame aims to be a simple 2D game development library targetted at beginner programmers who want to get started making games ASAP!

- Keep it simple!
- Keep it cross platform
- Make it easy to understand

Prebuilt SwinGame project templates can be downloaded from [SwinGame.com](http://swingame.com/index.php/downloads.html)

## File Organisation

SwinGame SDK is divided into multiple sections:

- **CoreSDK** contains the main code for the SwinGame API
    - This is written in Pascal
    - The backend links to SDL and OpenGL etc.
- **Dist** contains the files for distribution that are created by the scripts in Tools
- **Templates** contains the static files that are bundled with the code for distribution. These files are copied by the scripts in Tools into the Dist directory when built
- **Tools** contains the scripts that create the different distributions.

## Building SwinGame

Requires
- Free Pascal Compiler
- C/C++ Compiler
- bash
- Python 2.6
- C# Compiler (mono)

### Building the back end library

The back end library is a C/C++ project that links SwinGame to lower level libraries. Externally it exports a single function `sg_load` that loads and returns a struct that contains function pointers to the back end routines. This function is called by the front end library and the function pointers used to access routines in the linked libraries.

Before building the library you need to access the external projects. These can be downloaded using the `setup.sh` script in `Backend/SGSDL2/external`. You also need to access the static libraries related to these. On MacOS the static libraries are stored within the project at `Backend\SGSDL2\lib\mac` (these need to be kept up to date as SDL changes). With Linux and Windows, these libraries can be installed using scripts.

1. Windows: using **MSYS2**
1.1. `pacman --needed -Sy bash pacman pacman-mirrors msys2-runtime`
1.2  `pacman -Su`
1.3  `pacman -S mingw32/mingw-w64-i686-SDL2 mingw64/mingw-w64-x86_64-SDL2 mingw64/mingw-w64-x86_64-SDL2_gfx mingw32/mingw-w64-i686-SDL2_gfx mingw32/mingw-w64-i686-SDL2_image mingw64/mingw-w64-x86_64-SDL2_image mingw32/mingw-w64-i686-SDL2_mixer mingw64/mingw-w64-x86_64-SDL2_mixer mingw32/mingw-w64-i686-SDL2_net mingw64/mingw-w64-x86_64-SDL2_net mingw32/mingw-w64-i686-SDL2_ttf mingw64/mingw-w64-x86_64-SDL2_ttf mingw32/mingw-w64-i686-curl mingw64/mingw-w64-x86_64-curl pkg-config msys/libcurl-devel`
1.4 Replace the libSDL2_Mixer.a and related files with the **Developer** versions from SDL website
1.4 Replace libcurl libraries with those from libcurl website
1.5 Compile 32 and 64 bit versions from `Backend/SGSDL2/projects/bash`
2. Linux:
1.1 `apt-get install 

On MacOS use the provided XCode project at `Backend/SGSDL2/projects/XCode`, build the static library it contains and archive. Extract the static library from the archive and place in `CoreSDK/staticlib/mac`

On Linux use the `build.sh` bash script in `Backend

### Building the front end library
All the libraries are built from the command line (using bash - install MSYS on windows).

The building scripts are in Tools/Scripts.

To build run the following:

    python create_library.sh
    python bundle_templates.sh
    python produce_templates

## Testing Features
Testing new features:

This can be done with the ./build script in CoreSDK (on Mac only at the moment).

## Project Template Structures
Structure of a template:

./
./bin for compiled game output
./lib includes SwinGame source and libraries
./tmp for temporary output of the compiler
./Resources includes SwinGame resources (images, sounds, etc)
./src Source code for game

Of these Resources needs to retain the same structure to allow SwinGame to load data correctly.

## Adding a new language
Adding new templates to a language:

1: Create a project using the environment desired.
2: Check that it works :)
3: Remove shared folders including those in:
3.1: Templates/Common/Resources
3.2: Templates/${lang}/Common
4: Place template directory in /Templates/${lang}/${template kind}
5: Alter bundle_${lang}_templates.sh to include template for appropriate platforms

## Contribution
There are always many things that need to be done to improve SwinGame. If you are interesting in contributing please get in touch.

Andrew Cain
acain@swin.edu.au