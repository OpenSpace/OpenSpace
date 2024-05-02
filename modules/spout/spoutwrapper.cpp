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

#include "modules/spout/spoutwrapper.h"

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#define SPOUT_NO_GL_INCLUDE
#include <SpoutLibrary.h>

namespace {
    constexpr std::string_view _loggerCat = "Spout";

    constexpr openspace::properties::Property::PropertyInfo NameSenderInfo = {
        "SpoutName",
        "Spout Sender Name",
        "This value sets the Spout sender to use a specific name.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo NameReceiverInfo = {
        "SpoutName",
        "Spout Receiver Name",
        "This value explicitly sets the Spout receiver to use a specific name. If this "
        "is not a valid name, the first Spout image is used instead",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "SpoutSelection",
        "Spout Selection",
        "This property displays all available Spout sender on the system. If one them is "
        "selected, its value is stored in the 'SpoutName' property, overwriting its "
        "previous value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UpdateInfo = {
        "UpdateSelection",
        "Update Selection",
        "If this property is trigged, the 'SpoutSelection' options will be refreshed.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(SpoutReceiver)]] ReceiverParameters {
        // [[codegen::verbatim(NameReceiverInfo.description)]]
        std::optional<std::string> spoutName;
    };

    struct [[codegen::Dictionary(SpoutSender)]] SenderParameters {
        // [[codegen::verbatim(NameSenderInfo.description)]]
        std::string spoutName;
    };

#include "spoutwrapper_codegen.cpp"
} // namespace

namespace openspace::spout {

SpoutMain::SpoutMain() {
    _spoutHandle = GetSpout();
}

SpoutMain::~SpoutMain() {}

void SpoutMain::release() {
    if (_spoutHandle) {
        _spoutHandle->Release();
    }
}

void SpoutMain::saveGLState() {
    GLint buf;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &buf);
    _defaultFBO = static_cast<unsigned int>(buf);

    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &buf);
    _defaultReadFBO = static_cast<unsigned int>(buf);

    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &buf);
    _defaultDrawFBO = static_cast<unsigned int>(buf);

    glGetIntegerv(GL_READ_BUFFER, &buf);
    _defaultReadBuffer = static_cast<unsigned int>(buf);

    glGetIntegerv(GL_DRAW_BUFFER0, &buf);
    _defaultDrawBuffer[0] = static_cast<unsigned int>(buf);

    saveGLTextureState();
}

void SpoutMain::restoreGLState() {
    glBindFramebuffer(GL_FRAMEBUFFER, static_cast<GLuint>(_defaultFBO));
    if (_defaultFBO) {
        glBindFramebuffer(GL_READ_FRAMEBUFFER, static_cast<GLuint>(_defaultReadFBO));
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, static_cast<GLuint>(_defaultDrawFBO));
        glReadBuffer(static_cast<GLenum>(_defaultReadBuffer));
        GLenum buf[1];
        buf[0] = static_cast<GLenum>(_defaultDrawBuffer[0]);
        glDrawBuffers(1, buf);
    }
    restoreGLTextureState();
}

void SpoutMain::saveGLTextureState() {
    GLint buf;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, &buf);
    _defaultTexture = static_cast<unsigned int>(buf);
}

void SpoutMain::restoreGLTextureState() {
    glBindTexture(GL_TEXTURE_2D, static_cast<GLuint>(_defaultTexture));
}

SpoutReceiver::SpoutReceiver() {}

SpoutReceiver::~SpoutReceiver() {}

const std::vector<std::string>& SpoutReceiver::spoutReceiverList() {
    if (!_spoutHandle) {
        return _receiverList;
    }

    const int nSenders = _spoutHandle->GetSenderCount();
    _receiverList.clear();

    for (int i = 0; i < nSenders; i++) {
        char Name[256];
        _spoutHandle->GetSenderName(i, Name, 256);
        _receiverList.push_back(Name);
    }

    return _receiverList;
}

bool SpoutReceiver::isCreated() const {
    return _isCreated;
}

bool SpoutReceiver::isReceiving() const {
    return _isReceiving;
}

bool SpoutReceiver::updateReceiver() {
    unsigned int width = 10;
    unsigned int height = 10;

    if (!_spoutHandle || !_isCreated) {
        return false;
    }

    char currentSpoutName[256] = { 0 };
    std::memcpy(currentSpoutName, _currentSpoutName.data(), _currentSpoutName.size());
    _spoutHandle->CheckReceiver(currentSpoutName, width, height, _isReceiving);

    // if spout is not connected a 10x10 texture is created
    if (updateTexture(width, height) && _isReceiving) {
        saveGLState();

        _spoutHandle->ReceiveTexture(
            currentSpoutName,
            width,
            height,
            static_cast<GLuint>(*_spoutTexture),
            static_cast<GLuint>(GL_TEXTURE_2D),
            true
        );

        if (_onUpdateReceiverCallback) {
            const GLuint t = static_cast<GLuint>(*_spoutTexture);
            if (!_onUpdateReceiverCallback(width, height, t)) {
                restoreGLState();
                return false;
            }
        }

        restoreGLState();
        return true;
    }

    return false;
}

bool SpoutReceiver::updateReceiverName(const std::string& name) {
    unsigned int width = 0;
    unsigned int height = 0;

    if (!_spoutHandle) {
        return false;
    }

    releaseReceiver();

    if (_onUpdateReceiverNameCallback) {
        if (!_onUpdateReceiverNameCallback(name)) {
            return false;
        }
    }

    char nameBuf[256] = { 0 };
    std::memcpy(nameBuf, name.data(), name.size());
    bool hasCreated = _spoutHandle->CreateReceiver(nameBuf, width, height);
    if (!hasCreated) {
        if (!_isErrorMessageDisplayed) {
            LWARNING(std::format(
                "Could not create receiver for {} -> {}x{}", name, width, height
            ));
            _isErrorMessageDisplayed = true;
        }
        return false;
    }

    _currentSpoutName = name;
    _isErrorMessageDisplayed = false;
    _isCreated = true;

    return true;
}

void SpoutReceiver::releaseReceiver() {
    if (!_isCreated) {
        return;
    }

    _isReceiving = false;
    _isCreated = false;
    _isErrorMessageDisplayed = false;
    _currentSpoutName.clear();
    if (_onReleaseReceiverCallback) {
        _onReleaseReceiverCallback();
    }
    releaseTexture();
    if (_spoutHandle) {
        _spoutHandle->ReleaseReceiver();
    }
}

void SpoutReceiver::release() {
    releaseReceiver();
    SpoutMain::release();
}

void SpoutReceiver::onUpdateReceiverName(std::function<bool(const std::string&)> callback)
{
    _onUpdateReceiverNameCallback = std::move(callback);
}

void SpoutReceiver::onUpdateReceiver(std::function<bool(int, int, unsigned int)> callback)
{
    _onUpdateReceiverCallback = std::move(callback);
}

void SpoutReceiver::onReleaseReceiver(std::function<void()> callback) {
    _onReleaseReceiverCallback = std::move(callback);
}

void SpoutReceiver::onUpdateTexture(std::function<bool(int, int)> callback) {
    _onUpdateTextureCallback = std::move(callback);
}

void SpoutReceiver::onReleaseTexture(std::function<void()> callback) {
    _onReleaseTextureCallback = std::move(callback);
}

unsigned int SpoutReceiver::spoutTexture() const {
    return _spoutTexture ? static_cast<unsigned int>(*_spoutTexture) : 0;
}

bool SpoutReceiver::updateTexture(unsigned int width, unsigned int height) {
    if (width != _spoutWidth || height != _spoutHeight) {
        releaseTexture();
        _spoutTexture = std::make_unique<ghoul::opengl::Texture>(
            glm::uvec3(width, height, 1),
            GL_TEXTURE_2D,
            ghoul::opengl::Texture::Format::RGBA,
            GL_RGBA, GL_UNSIGNED_BYTE,
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::Repeat,
            ghoul::opengl::Texture::AllocateData::No,
            ghoul::opengl::Texture::TakeOwnership::No
        );

        if (_spoutTexture) {
            _spoutTexture->uploadTexture();
            if (_onUpdateTextureCallback && !_onUpdateTextureCallback(width, height)) {
                LWARNING(std::format(
                    "Could not create callback texture for {} -> {}x{}",
                    _currentSpoutName, width, height
                ));
                return false;
            }
            _spoutWidth = width;
            _spoutHeight = height;
        }
        else {
            LWARNING(std::format(
                "Could not create texture for {} -> {}x{}",
                _currentSpoutName, width, height
            ));
            return false;
        }
    }
    return true;
}

void SpoutReceiver::releaseTexture() {
    _spoutWidth = 0;
    _spoutHeight = 0;
    if (_onReleaseTextureCallback) {
        _onReleaseTextureCallback();
    }
    _spoutTexture.release();
}

const properties::Property::PropertyInfo& SpoutReceiverPropertyProxy::NameInfoProperty() {
    return NameReceiverInfo;
}

const properties::Property::PropertyInfo&
SpoutReceiverPropertyProxy::SelectionInfoProperty()
{
    return SelectionInfo;
}

const properties::Property::PropertyInfo& SpoutReceiverPropertyProxy::UpdateInfoProperty()
{
    return UpdateInfo;
}

documentation::Documentation SpoutReceiverPropertyProxy::Documentation() {
    return codegen::doc<ReceiverParameters>("spout_receiver");
}

SpoutReceiverPropertyProxy::SpoutReceiverPropertyProxy(properties::PropertyOwner& owner,
                                                      const ghoul::Dictionary& dictionary)
    : _spoutName(NameReceiverInfo)
    , _spoutSelection(SelectionInfo)
    , _updateSelection(UpdateInfo)
{
    const ReceiverParameters p = codegen::bake<ReceiverParameters>(dictionary);

    _spoutName = p.spoutName.value_or(_spoutName);
    _isSelectAny = !p.spoutName.has_value();

    _spoutName.onChange([this]() { _isSpoutDirty = true; });
    owner.addProperty(_spoutName);

    _spoutSelection.onChange([this]() {
        if (_spoutName.value().empty() && _spoutSelection.value() == 0) {
            if (_spoutSelection.options().size() > 1) {
                _spoutSelection = 1;
            }
        }
        _spoutName = "";
        _spoutName = _spoutSelection.option().description;
    });
    _spoutSelection.addOption(0, "");
    owner.addProperty(_spoutSelection);

    _updateSelection.onChange([this]() {
        const std::vector<std::string> receiverList = spoutReceiverList();

        _spoutSelection.clearOptions();
        _spoutSelection.addOption(0, "");

        int idx = 0;
        for (int i = 0; i < static_cast<int>(receiverList.size()); i++) {
            _spoutSelection.addOption(i + 1, receiverList[i]);

            LWARNING(std::format("List {}", receiverList[i]));

            if (!_isSelectAny && _spoutName.value() == receiverList[i]) {
                idx = i + 1;
            }
        }
        _spoutSelection = idx;

    });
    owner.addProperty(_updateSelection);

    _updateSelection.trigger();
}

SpoutReceiverPropertyProxy::~SpoutReceiverPropertyProxy() {}

bool SpoutReceiverPropertyProxy::updateReceiver() {
    if (_isSpoutDirty) {
        if (!updateReceiverName(_spoutName.value())) {
            return false;
        }
        _isSpoutDirty = false;
    }
    return SpoutReceiver::updateReceiver();
}

void SpoutReceiverPropertyProxy::releaseReceiver() {
    _isSpoutDirty = true;
    SpoutReceiver::releaseReceiver();
}

SpoutSender::SpoutSender() {}

SpoutSender::~SpoutSender() {}

bool SpoutSender::isCreated() const {
    return _isCreated;
}

bool SpoutSender::isSending() const {
    return _isSending;
}

bool SpoutSender::updateSenderStatus() {
    if (!_isSending) {
        if (_spoutWidth == 0 || _spoutHeight == 0) {
            if (!_isErrorMessageDisplayed) {
                LWARNING(std::format(
                    "Could not create sender for {}, dimensions invalid {}x{}",
                    _currentSpoutName, _spoutWidth, _spoutHeight
                ));
                _isErrorMessageDisplayed = true;
            }
            return false;
        }

        if (_currentSpoutName.empty()) {
            if (!_isErrorMessageDisplayed) {
                LWARNING(std::format("Could not create sender, invalid name"));
                _isErrorMessageDisplayed = true;
            }
            return false;
        }

        ghoul_assert(_currentSpoutName.size() < 256, "Spout name must be < 256");
        char name[256] = { 0 };
        std::memcpy(name, _currentSpoutName.data(), _currentSpoutName.size());

        bool hasCreated = _spoutHandle->CreateSender(name, _spoutWidth, _spoutHeight);
        if (!hasCreated) {
            if (!_isErrorMessageDisplayed) {
                LWARNING(std::format(
                    "Could not create sender for {} -> {}x{}",
                    _currentSpoutName, _spoutWidth, _spoutHeight
                ));
                _isErrorMessageDisplayed = true;
            }
            return false;
        }
    }

    _isErrorMessageDisplayed = false;
    _isSending = true;

    return true;
}

bool SpoutSender::updateSender(unsigned int texture, unsigned int textureType) {
    if (!_spoutHandle || !updateSenderStatus()) {
        return false;
    }

    _spoutHandle->SendTexture(texture, textureType, _spoutWidth, _spoutHeight);

    if (_onUpdateSenderCallback) {
        bool s = _onUpdateSenderCallback(
            _currentSpoutName,
            texture,
            textureType,
            _spoutWidth,
            _spoutHeight
        );
        if (!s) {
            return false;
        }
    }

    return true;
}

bool SpoutSender::updateSenderName(const std::string& name) {
    if (!_spoutHandle) {
        return false;
    }
    if (name == _currentSpoutName) {
        return true;
    }

    releaseSender();

    if (_onUpdateSenderNameCallback) {
        if (!_onUpdateSenderNameCallback(name)) {
            return false;
        }
    }

    _currentSpoutName = name;
    _isCreated = true;

    return true;
}

bool SpoutSender::updateSenderSize(int width, int height) {
    if (!_spoutHandle) {
        return false;
    }
    if (width == static_cast<int>(_spoutWidth) &&
        height == static_cast<int>(_spoutHeight))
    {
        return true;
    }

    releaseSender();

    if (_onUpdateSenderSizeCallback) {
        if (!_onUpdateSenderSizeCallback(width, height)) {
            return false;
        }
    }

    _spoutWidth = width;
    _spoutHeight = height;
    _isCreated = true;

    return true;
}

void SpoutSender::releaseSender() {
    if (!_isSending) {
        return;
    }

    _isCreated = false;
    _isSending = false;
    _isErrorMessageDisplayed = false;
    _currentSpoutName.clear();
    _spoutWidth = 0;
    _spoutHeight = 0;
    if (_onReleaseSenderCallback) {
        _onReleaseSenderCallback();
    }
    if (_spoutHandle) {
        _spoutHandle->ReleaseReceiver();
    }
}

void SpoutSender::release() {
    releaseSender();
    SpoutMain::release();
}

void SpoutSender::onUpdateSenderName(std::function<bool(const std::string&)> callback) {
    _onUpdateSenderNameCallback = std::move(callback);
}

void SpoutSender::onUpdateSenderSize(std::function<bool(int, int)> callback) {
    _onUpdateSenderSizeCallback = std::move(callback);
}

void SpoutSender::onUpdateSender(std::function<bool(const std::string&, unsigned int,
                                                    unsigned int, int, int)> callback)
{
    _onUpdateSenderCallback = std::move(callback);
}

void SpoutSender::onReleaseSender(std::function<void()> callback) {
    _onReleaseSenderCallback = std::move(callback);
}

const properties::Property::PropertyInfo& SpoutSenderPropertyProxy::NameInfoProperty() {
    return NameSenderInfo;
}

documentation::Documentation SpoutSenderPropertyProxy::Documentation() {
    return codegen::doc<SenderParameters>("spout_sender");
}

SpoutSenderPropertyProxy::SpoutSenderPropertyProxy(properties::PropertyOwner& owner,
                                                   const ghoul::Dictionary& dictionary)
    : _spoutName(NameSenderInfo)
{
    const SenderParameters p = codegen::bake<SenderParameters>(dictionary);

    _spoutName = p.spoutName;
    _spoutName.onChange([this]() { _isSpoutDirty = true; });
    owner.addProperty(_spoutName);
}

SpoutSenderPropertyProxy::~SpoutSenderPropertyProxy() {}

bool SpoutSenderPropertyProxy::updateSender(unsigned int texture,
                                            unsigned int textureType)
{
    if (_isSpoutDirty) {
        if (!updateSenderName(_spoutName)) {
            return false;
        }
        _isSpoutDirty = false;
    }
    return SpoutSender::updateSender(texture, textureType);
}

void SpoutSenderPropertyProxy::releaseSender() {
    _isSpoutDirty = true;
    SpoutSender::releaseSender();
}

} // namespace openspace::spout
