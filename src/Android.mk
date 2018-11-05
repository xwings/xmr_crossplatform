LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := xmr-stak
LOCAL_LDFLAGS :=
#LOCAL_LDLIBS := -lpthread
LOCAL_CFLAGS := -DBACKEND_TYPE=cpu -DCONF_NO_CUDA -DCONF_NO_OPENCL -DCONF_NO_HWLOC -DCONF_NO_TLS -DCONF_NO_HTTPD -DNDEBUG -DUSE_C_PTHREAD -Os -std=gnu99
LOCAL_CPPFLAGS := -DBACKEND_TYPE=cpu -DCONF_NO_CUDA -DCONF_NO_OPENCL -DCONF_NO_HWLOC -DCONF_NO_TLS -DCONF_NO_HTTPD -DNDEBUG -DUSE_C_PTHREAD -Os -std=c++11

LOCAL_SRC_FILES := c_blake256.c  c_groestl.c  c_jh.c  c_keccak.c  c_skein.c \
backendConnector.cpp  cli-miner.cpp  console.cpp  jconf_cpu.cpp  cryptonight_common.cpp  executor.cpp  globalStates.cpp  hwlocMemory.cpp  jpsock.cpp  minethd.cpp  socket.cpp  telemetry.cpp  uac.cpp  utility.cpp  jconf.cpp  version.cpp

include $(BUILD_EXECUTABLE)
