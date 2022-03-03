/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_APP_OPENSPACE___SPOUTWRAPPER___H__
#define __OPENSPACE_APP_OPENSPACE___SPOUTWRAPPER___H__

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/propertyowner.h>
#include <string>
#include <vector>

struct SPOUTLIBRARY;
typedef SPOUTLIBRARY* SPOUTHANDLE;

namespace ghoul::opengl {
    class Texture;
}; // namespace ghoul::opengl

namespace openspace::spout {

// @TODO(abock, 2022-03-02)  This class should probably be outsourced into a stand-alone
//                           library that the SGCT version of this class then can also use
class SpoutMain {
public:
    SpoutMain();
    virtual ~SpoutMain();

    virtual void Release();

    void SaveGLState();
    void RestoreGLState();

    void SaveGLTextureState();
    void RestoreGLTextureState();

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

    void Release() override;

    virtual bool UpdateReceiverName(const std::string& name);
    virtual bool UpdateReceiver();
    virtual void ReleaseReceiver();

    void OnUpdateReceiverName(std::function<bool(const std::string&)> callback);
    void OnUpdateReceiver(std::function<bool(int, int, unsigned int)> callback);
    void OnReleaseReceiver(std::function<void()> callback);
    void OnUpdateTexture(std::function<bool(int, int)> callback);
    void OnReleaseTexture(std::function<void()> callback);

    const std::vector<std::string>& SpoutReceiverList();
    bool isCreated() const;
    bool isReceiving() const;
    unsigned int SpoutTexture() const;

private:
    bool UpdateTexture(unsigned int width, unsigned int height);
    void ReleaseTexture();

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

    bool UpdateReceiver() override;
    void ReleaseReceiver() override;

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

    void Release() override;

    virtual bool UpdateSenderName(const std::string& name);
    virtual bool UpdateSenderSize(int width, int height);
    virtual bool UpdateSender(unsigned int texture, unsigned int textureType);
    virtual void ReleaseSender();

    void OnUpdateSenderName(std::function<bool(const std::string&)> callback);
    void OnUpdateSenderSize(std::function<bool(int, int)> callback);
    void OnUpdateSender(std::function<bool(const std::string&, unsigned int,
                                           unsigned int, int, int)> callback);
    void OnReleaseSender(std::function<void()> callback);

    bool isCreated() const;
    bool isSending() const;

private:
    bool _isErrorMessageDisplayed = false;
    bool _isCreated = false;
    bool _isSending = false;

    std::function<bool(const std::string&)> _onUpdateSenderNameCallback = nullptr;
    std::function<bool(int, int)> _onUpdateSenderSizeCallback = nullptr;
    std::function<bool(const std::string&, unsigned int,
        unsigned int, int, int)> _onUpdateSenderCallback = nullptr;
    std::function<void()> _onReleaseSenderCallback = nullptr;

    bool UpdateSenderStatus();
};

class SpoutSenderPropertyProxy : public SpoutSender {
public:
    static const properties::Property::PropertyInfo& NameInfoProperty();

    SpoutSenderPropertyProxy(properties::PropertyOwner& owner,
        const ghoul::Dictionary& dictionary);
    virtual ~SpoutSenderPropertyProxy();

    bool UpdateSender(unsigned int texture, unsigned int textureType) override;
    void ReleaseSender() override;

private:
    properties::StringProperty _spoutName;

    bool _isSpoutDirty = true;
};

} // namespace openspace::spout

#endif // __OPENSPACE_APP_OPENSPACE___SPOUTWRAPPER___H__
