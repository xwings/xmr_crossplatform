#CC = mipsel-openwrt-linux-gcc
#CXX = mipsel-openwrt-linux-g++
#CC = arm-linux-gnueabihf-gcc
#CXX = arm-linux-gnueabihf-g++
#CC = /opt/arm/armv7l-cortex-a9-soft/bin/arm-linux-gnueabi-gcc
#CXX = /opt/arm/armv7l-cortex-a9-soft/bin/arm-linux-gnueabi-g++
#CC = /opt/arm/armv5l-xscale-soft/bin/arm-linux-gnueabi-gcc
#CXX = /opt/arm/armv5l-xscale-soft/bin/arm-linux-gnueabi-g++
CC = gcc
CXX = g++
CFLAGS = -Wall -Os
CFLAGS += -DBACKEND_TYPE=cpu -DCONF_NO_CUDA -DCONF_NO_OPENCL -DCONF_NO_HWLOC -DCONF_NO_TLS -DCONF_NO_HTTPD
CFLAGS += -DUSE_C_PTHREAD
CFLAGS += -Wno-strict-aliasing
#CFLAGS += -maes

CXX_FLAGS = $(CFLAGS)
CXX_FLAGS += -std=c++11

OBJS = $(patsubst %.c,%.o,$(wildcard *.c))
DEPS = $(patsubst %.o,%.d,$(OBJS))
CFLAGS += -MD
MISSING_DEPS = $(filter-out $(wildcard $(DEPS)),$(DEPS))
MISSING_DEPS_SOURCES = $(wildcard $(patsubst %.d,%.c,$(MISSING_DEPS)))

CXX_OBJS = $(patsubst %.cpp,%.o,$(wildcard *.cpp))
CXX_DEPS = $(patsubst %.o,%.d,$(CXX_OBJS))
CXX_MISSING_DEPS = $(filter-out $(wildcard $(CXX_DEPS)),$(cXX_DEPS))
CXX_MISSING_DEPS_SOURCES = $(wildcard $(patsubst %.d,%.cpp,$(CXX_MISSING_DEPS)))

LIBS = -lpthread -latomic

TARGET = xmr-stak


# android build
#ARCH = $(shell adb shell getprop ro.product.cpu.abi)
#SDK_VERSION = $(shell adb shell getprop ro.build.version.sdk)

# android-4.4.4
ARCH = armeabi-v7a
SDK_VERSION = 19

NDK_BUILD = /opt/android-ndk/ndk-build

all:$(TARGET)

ifneq ($(MISSING_DEPS),)
$(MISSING_DEPS) :
	@$(RM) $(patsubst %.d,%.o,$@)
endif
-include $(DEPS)

ifneq ($(CXX_MISSING_DEPS),)
$(CXX_MISSING_DEPS) :
	@$(RM) $(patsubst %.d,%.o,$@)
endif
-include $(CXX_DEPS)


$(TARGET):$(OBJS) $(CXX_OBJS)
#	$(CXX) $(CFLAGS) -o $(TARGET) $^ $(LIBS)
#	$(CXX) -static $(CXX_FLAGS) -o $(TARGET) $^ $(LIBS)
	$(CXX) $(CXX_FLAGS) -o $(TARGET) $^ $(LIBS)

$(OBJS):%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(CXX_OBJS):%.o:%.cpp
	$(CXX) $(CXX_FLAGS) -c $< -o $@

android:
	$(NDK_BUILD) NDK_PROJECT_PATH=. APP_BUILD_SCRIPT=./Android.mk APP_ABI=$(ARCH) APP_PLATFORM=android-$(SDK_VERSION)

clean:
	rm -rf obj libs
	rm -f *.d *.s *.o $(TARGET)
