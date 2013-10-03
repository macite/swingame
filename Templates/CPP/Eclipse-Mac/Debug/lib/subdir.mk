################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../lib/Animations.cpp \
../lib/Audio.cpp \
../lib/Camera.cpp \
../lib/Characters.cpp \
../lib/Colors.cpp \
../lib/Geometry.cpp \
../lib/Graphics.cpp \
../lib/Images.cpp \
../lib/Input.cpp \
../lib/Networking.cpp \
../lib/Physics.cpp \
../lib/Resources.cpp \
../lib/SGSDK.cpp \
../lib/Sprites.cpp \
../lib/Text.cpp \
../lib/Timers.cpp \
../lib/UserInterface.cpp \
../lib/Utils.cpp 

OBJS += \
./lib/Animations.o \
./lib/Audio.o \
./lib/Camera.o \
./lib/Characters.o \
./lib/Colors.o \
./lib/Geometry.o \
./lib/Graphics.o \
./lib/Images.o \
./lib/Input.o \
./lib/Networking.o \
./lib/Physics.o \
./lib/Resources.o \
./lib/SGSDK.o \
./lib/Sprites.o \
./lib/Text.o \
./lib/Timers.o \
./lib/UserInterface.o \
./lib/Utils.o 

C_DEPS += \
./lib/Animations.d \
./lib/Audio.d \
./lib/Camera.d \
./lib/Characters.d \
./lib/Colors.d \
./lib/Geometry.d \
./lib/Graphics.d \
./lib/Images.d \
./lib/Input.d \
./lib/Networking.d \
./lib/Physics.d \
./lib/Resources.d \
./lib/SGSDK.d \
./lib/Sprites.d \
./lib/Text.d \
./lib/Timers.d \
./lib/UserInterface.d \
./lib/Utils.d 


# Each subdirectory must supply rules for building sources it contributes
lib/%.o: ../lib/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	g++ -O0 -g3 -Wall -c -fmessage-length=0 -arch i386 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


