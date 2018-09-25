# xmr-crossplatform

    非x86平台挖矿。
    强制使用CPU後端，将x86专用代码改写成可移植。

## origin

    https://github.com/fireice-uk/xmr-stak

## tested

    支持GNU toolchain(gcc + glibc + binutils)。
    已测试平台raspberry pi 2b(arm)
    已测试平台raspberry pi 3b(aarch64)

## todo

    测试更多的架构平台以及系统的ABI兼容。
    精简二进制程序大小。
    部分设备运行环境仅支持老版本的工具链，改写部分C++11代码。