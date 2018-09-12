#CC = arm-linux-gnueabihf-gcc
#CXX = arm-linux-gnueabihf-g++
#CC = mipsel-openwrt-linux-gcc
#CXX = mipsel-openwrt-linux-g++
CC = gcc
CXX = g++
CFLAGS = -Wall -O2
CFLAGS += -DBACKEND_TYPE=cpu -DCONF_NO_CUDA -DCONF_NO_OPENCL -DCONF_NO_HWLOC -DCONF_NO_TLS -DCONF_NO_HTTPD -DNDEBUG
#CFLAGS += -DGIT_BRANCH=master -DGIT_COMMIT_HASH=c5f0505
CFLAGS += -Wall -O2 -Os

OBJS = $(patsubst %.c,%.o,$(wildcard *.c))
DEPS = $(patsubst %.o,%.d,$(OBJS))
CFLAGS += -MD
MISSING_DEPS = $(filter-out $(wildcard $(DEPS)),$(DEPS))
MISSING_DEPS_SOURCES = $(wildcard $(patsubst %.d,%.c,$(MISSING_DEPS)))

CXX_OBJS = $(patsubst %.cpp,%.o,$(wildcard *.cpp))
CXX_DEPS = $(patsubst %.o,%.d,$(CXX_OBJS))
CXX_MISSING_DEPS = $(filter-out $(wildcard $(CXX_DEPS)),$(cXX_DEPS))
CXX_MISSING_DEPS_SOURCES = $(wildcard $(patsubst %.d,%.cpp,$(CXX_MISSING_DEPS)))

LIBS = -lpthread

TARGET = xmr-stak

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
	$(CXX) $(CFLAGS) -o $(TARGET) $^ $(LIBS)

$(OBJS):%.o:%.c
	$(CC) $(CFLAGS) -std=gnu99 -c $< -o $@

$(CXX_OBJS):%.o:%.cpp
	$(CXX) $(CFLAGS) -std=c++11 -c $< -o $@

clean:
	rm -f *.d *.s *.o $(TARGET)
