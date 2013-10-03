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

Requires bash and Python 2.6

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