#pragma once
#ifdef WIN32

class Win32TouchHook {
public:
    Win32TouchHook(void* nativeWindowPtr);
    ~Win32TouchHook();

private:
    bool _enabled;
};

#endif //WIN32
