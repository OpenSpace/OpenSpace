/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_MODULE_SPOUT___SPOUTWRAPPER___H__
#define __OPENSPACE_MODULE_SPOUT___SPOUTWRAPPER___H__

#include <openspace/documentation/documentation.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <string>
#include <vector>

struct SPOUTLIBRARY;
typedef SPOUTLIBRARY* SPOUTHANDLE;

namespace ghoul::opengl { class Texture; }

namespace openspace::spout {

// @TODO(abock, 2022-03-02)  This class should probably be outsourced into a stand-alone
//                           library that the SGCT version of this class then can also use
class SpoutMain {
public:
    SpoutMain();
    virtual ~SpoutMain();

    virtual void release();

    void saveGLState();
    void restoreGLState();

    void saveGLTextureState();
    void restoreGLTextureState();

protected:
    unsigned int _defaultTexture;
    unsigned int _defaultFBO;
    unsigned int _defaultReadFBO;
    unsigned int _defaultDrawFBO;
    unsigned int _defaultReadBuffer;
    unsigned int _defaultDrawBuffer[1] = { 0 };

    SPOUTHANDLE _spoutHandle = nullptr;

    std::string _currentSpoutName;
    unsigned int _spoutWidth = 0;
    unsigned int _spoutHeight = 0;
};

class SpoutReceiver : public SpoutMain {
public:
    SpoutReceiver();
    virtual ~SpoutReceiver();

    void release() override;

    virtual bool updateReceiverName(const std::string& name);
    virtual bool updateReceiver();
    virtual void releaseReceiver();

    void onUpdateReceiverName(std::function<bool(const std::string&)> callback);
    void onUpdateReceiver(std::function<bool(int, int, unsigned int)> callback);
    void onReleaseReceiver(std::function<void()> callback);
    void onUpdateTexture(std::function<bool(int, int)> callback);
    void onReleaseTexture(std::function<void()> callback);

    const std::vector<std::string>& spoutReceiverList();
    bool isCreated() const;
    bool isReceiving() const;
    unsigned int spoutTexture() const;

private:
    bool updateTexture(unsigned int width, unsigned int height);
    void releaseTexture();

    bool _isErrorMessageDisplayed = false;
    bool _isCreated = false;
    bool _isReceiving = false;
    std::vector<std::string> _receiverList;

    std::unique_ptr<ghoul::opengl::Texture> _spoutTexture;

    std::function<bool(const std::string&)> _onUpdateReceiverNameCallback = nullptr;
    std::function<bool(int, int, unsigned int)> _onUpdateReceiverCallback = nullptr;
    std::function<void()> _onReleaseReceiverCallback = nullptr;
    std::function<bool(int, int)> _onUpdateTextureCallback = nullptr;
    std::function<void()> _onReleaseTextureCallback = nullptr;
};


class SpoutReceiverPropertyProxy : public SpoutReceiver {
public:
    static const properties::Property::PropertyInfo& NameInfoProperty();
    static const properties::Property::PropertyInfo& SelectionInfoProperty();
    static const properties::Property::PropertyInfo& UpdateInfoProperty();

    SpoutReceiverPropertyProxy(properties::PropertyOwner& owner,
        const ghoul::Dictionary& dictionary);
    virtual ~SpoutReceiverPropertyProxy();

    bool updateReceiver() override;
    void releaseReceiver() override;

    static documentation::Documentation Documentation();

private:
    properties::StringProperty _spoutName;
    properties::OptionProperty _spoutSelection;
    properties::TriggerProperty _updateSelection;

    bool _isSpoutDirty = true;
    bool _isSelectAny = false;
};


class SpoutSender : public SpoutMain {
public:
    SpoutSender();
    virtual ~SpoutSender();

    void release() override;

    virtual bool updateSenderName(const std::string& name);
    virtual bool updateSenderSize(int width, int height);
    virtual bool updateSender(unsigned int texture, unsigned int textureType);
    virtual void releaseSender();

    void onUpdateSenderName(std::function<bool(const std::string&)> callback);
    void onUpdateSenderSize(std::function<bool(int, int)> callback);
    void onUpdateSender(std::function<bool(const std::string&, unsigned int,
                                           unsigned int, int, int)> callback);
    void onReleaseSender(std::function<void()> callback);

    bool isCreated() const;
    bool isSending() const;

private:
    bool updateSenderStatus();

    bool _isErrorMessageDisplayed = false;
    bool _isCreated = false;
    bool _isSending = false;

    std::function<bool(const std::string&)> _onUpdateSenderNameCallback = nullptr;
    std::function<bool(int, int)> _onUpdateSenderSizeCallback = nullptr;
    std::function<bool(const std::string&, unsigned int,
        unsigned int, int, int)> _onUpdateSenderCallback = nullptr;
    std::function<void()> _onReleaseSenderCallback = nullptr;
};

class SpoutSenderPropertyProxy : public SpoutSender {
public:
    static const properties::Property::PropertyInfo& NameInfoProperty();

    SpoutSenderPropertyProxy(properties::PropertyOwner& owner,
        const ghoul::Dictionary& dictionary);
    virtual ~SpoutSenderPropertyProxy();

    bool updateSender(unsigned int texture, unsigned int textureType) override;
    void releaseSender() override;

    static documentation::Documentation Documentation();

private:
    properties::StringProperty _spoutName;

    bool _isSpoutDirty = true;
};

} // namespace openspace::spout

#endif // __OPENSPACE_MODULE_SPOUT___SPOUTWRAPPER___H__
