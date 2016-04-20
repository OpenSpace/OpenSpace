/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/onscreengui/include/gui.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <openspace/util/keys.h>


#include <openspace/engine/openspaceengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <openspace/properties/property.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <modules/base/rendering/screenspaceimage.h>
#include <modules/iswa/util/iswamanager.h>


#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

#include <imgui.h>

namespace {
	const std::string _loggerCat = "GUI";
	const std::string configurationFile = "imgui.ini";
	const ImVec2 size = ImVec2(350, 500);

	GLuint fontTex = 0;
	GLint positionLocation = 0;
	GLint uvLocation = 0;
	GLint colorLocation = 0;
	size_t vboMaxSize = 20000;
	GLuint vao = 0;
	GLuint vbo = 0;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

	static void ImImpl_RenderDrawLists(ImDrawList** const commandLists, int nCommandLists) {
		if (nCommandLists == 0)
			return;

		// Setup render state: alpha-blending enabled, no face culling, no depth testing, scissor enabled
		glEnable(GL_BLEND);
		glBlendEquation(GL_FUNC_ADD);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glDisable(GL_CULL_FACE);
		glDisable(GL_DEPTH_TEST);
		glEnable(GL_SCISSOR_TEST);

		ghoul::opengl::TextureUnit unit;
		unit.activate();
		glBindTexture(GL_TEXTURE_2D, fontTex);

		// Setup orthographic projection matrix
		const float width = ImGui::GetIO().DisplaySize.x;
		const float height = ImGui::GetIO().DisplaySize.y;
		const glm::mat4 ortho(
			2.f / width, 0.0f, 0.0f, 0.0f,
			0.0f, 2.0f / -height, 0.0f, 0.0f,
			0.0f, 0.0f, -1.0f, 0.0f,
			-1.0f, 1.0f, 0.0f, 1.0f
		);
		_program->activate();
		_program->setUniform("tex", unit);
		_program->setUniform("ortho", ortho);

		// Grow our buffer according to what we need
		size_t total_vtx_count = 0;
		for (int n = 0; n < nCommandLists; n++)
			total_vtx_count += commandLists[n]->vtx_buffer.size();
		glBindBuffer(GL_ARRAY_BUFFER, vbo);
		// @BUGFIX @HACK  The imgui crashes on complex scenes without the * 2 ---abock
		size_t neededBufferSize = total_vtx_count * sizeof(ImDrawVert) * 2;
		if (neededBufferSize > vboMaxSize) {
			vboMaxSize = neededBufferSize + 5000;  // Grow buffer
			glBufferData(GL_ARRAY_BUFFER, neededBufferSize, NULL, GL_STREAM_DRAW);
		}

		// Copy and convert all vertices into a single contiguous buffer
		unsigned char* buffer_data = (unsigned char*)glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
		if (!buffer_data)
			return;
		for (int n = 0; n < nCommandLists; ++n) {
			const ImDrawList* cmd_list = commandLists[n];
			memcpy(buffer_data, &cmd_list->vtx_buffer[0], cmd_list->vtx_buffer.size() * sizeof(ImDrawVert));
			buffer_data += cmd_list->vtx_buffer.size() * sizeof(ImDrawVert);
		}
		glUnmapBuffer(GL_ARRAY_BUFFER);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glBindVertexArray(vao);

		int cmd_offset = 0;
		for (int n = 0; n < nCommandLists; ++n) {
			const ImDrawList* cmd_list = commandLists[n];
			int vtx_offset = cmd_offset;
			for (auto pcmd : cmd_list->commands) {
				glScissor((int)pcmd.clip_rect.x, (int)(height - pcmd.clip_rect.w), (int)(pcmd.clip_rect.z - pcmd.clip_rect.x), (int)(pcmd.clip_rect.w - pcmd.clip_rect.y));
				glDrawArrays(GL_TRIANGLES, vtx_offset, pcmd.vtx_count);
				vtx_offset += pcmd.vtx_count;
			}
			cmd_offset = vtx_offset;
		}

		glBindVertexArray(0);
		_program->deactivate();
		glDisable(GL_SCISSOR_TEST);
		glBindTexture(GL_TEXTURE_2D, 0);
	}
}

namespace openspace {
	void addScreenSpaceRenderable(std::string texturePath){
		std::string filepath ="${OPENSPACE_DATA}/"+texturePath;
		if( ! FileSys.fileExists(filepath)) {
			LWARNING("Could not find image '" << filepath << "'");
			return;
		}
		OsEng.renderEngine().registerScreenSpaceRenderable(std::make_shared<ScreenSpaceImage>(filepath));
	}

namespace gui {

GUI::GUI() 
	: _isEnabled(false)
	, _showHelp(false)
{
}

GUI::~GUI() {
	ImGui::Shutdown();
}

bool GUI::isEnabled() const {
	return _isEnabled;
}

void GUI::setEnabled(bool enabled) {
	_isEnabled = enabled;
}

void GUI::initialize() {
	std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        configurationFile, "", ghoul::filesystem::CacheManager::Persistent::Yes
    );

	char* buffer = new char[cachedFile.size() + 1];

#ifdef WIN32
	strcpy_s(buffer, cachedFile.size() + 1, cachedFile.c_str());
#else
	strcpy(buffer, cachedFile.c_str());
#endif

	ImGuiIO& io = ImGui::GetIO();
	io.IniFilename = buffer;
	//io.IniSavingRate = 5.f;
	io.DeltaTime = 1.f / 60.f;
	//io.PixelCenterOffset = 0.5f;
    io.KeyMap[ImGuiKey_Tab] = static_cast<int>(Key::Tab);
    io.KeyMap[ImGuiKey_LeftArrow] = static_cast<int>(Key::Left);
	io.KeyMap[ImGuiKey_RightArrow] = static_cast<int>(Key::Right);
	io.KeyMap[ImGuiKey_UpArrow] = static_cast<int>(Key::Up);
	io.KeyMap[ImGuiKey_DownArrow] = static_cast<int>(Key::Down);
	io.KeyMap[ImGuiKey_Home] = static_cast<int>(Key::Home);
	io.KeyMap[ImGuiKey_End] = static_cast<int>(Key::End);
	io.KeyMap[ImGuiKey_Delete] = static_cast<int>(Key::Delete);
	io.KeyMap[ImGuiKey_Backspace] = static_cast<int>(Key::BackSpace);
	io.KeyMap[ImGuiKey_Enter] = static_cast<int>(Key::Enter);
	io.KeyMap[ImGuiKey_Escape] = static_cast<int>(Key::Escape);
	io.KeyMap[ImGuiKey_A] = static_cast<int>(Key::A);
	io.KeyMap[ImGuiKey_C] = static_cast<int>(Key::C);
	io.KeyMap[ImGuiKey_V] = static_cast<int>(Key::V);
	io.KeyMap[ImGuiKey_X] = static_cast<int>(Key::X);
	io.KeyMap[ImGuiKey_Y] = static_cast<int>(Key::Y);
	io.KeyMap[ImGuiKey_Z] = static_cast<int>(Key::Z);

	io.RenderDrawListsFn = ImImpl_RenderDrawLists;
	//io.SetClipboardTextFn = ImImpl_SetClipboardTextFn; // @TODO implement? ---abock
	//io.GetClipboardTextFn = ImImpl_GetClipboardTextFn; // @TODO implement? ---abock

	_property.initialize();
	_iSWAproperty.initialize();
	_screenSpaceProperty.initialize();
	_performance.initialize();
	_help.initialize();
}

void GUI::initializeGL() {
	_program = ghoul::opengl::ProgramObject::Build(
        "GUI",
		"${MODULE_ONSCREENGUI}/shaders/gui_vs.glsl",
        "${MODULE_ONSCREENGUI}/shaders/gui_fs.glsl"
    );
    if (!_program)
        return;

	positionLocation = glGetAttribLocation(*_program, "in_position");
	uvLocation = glGetAttribLocation(*_program, "in_uv");
	colorLocation = glGetAttribLocation(*_program, "in_color");
    
    glGenTextures(1, &fontTex);
	glBindTexture(GL_TEXTURE_2D, fontTex);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	unsigned char* png_data;
	int tex_x, tex_y;
    ImGui::GetIO().Fonts->GetTexDataAsRGBA32(&png_data, &tex_x, &tex_y);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_x, tex_y, 0, GL_RGBA, GL_UNSIGNED_BYTE, png_data);
	//stbi_image_free(tex_data);

	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, vboMaxSize, NULL, GL_DYNAMIC_DRAW);

	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glEnableVertexAttribArray(positionLocation);
	glEnableVertexAttribArray(uvLocation);
	glEnableVertexAttribArray(colorLocation);

	glVertexAttribPointer(positionLocation, 2, GL_FLOAT, GL_FALSE, sizeof(ImDrawVert), (GLvoid*)offsetof(ImDrawVert, pos));
	glVertexAttribPointer(uvLocation, 2, GL_FLOAT, GL_FALSE, sizeof(ImDrawVert), (GLvoid*)offsetof(ImDrawVert, uv));
	glVertexAttribPointer(colorLocation, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(ImDrawVert), (GLvoid*)offsetof(ImDrawVert, col));
	glBindVertexArray(0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
    

	_property.initializeGL();
	_iSWAproperty.initializeGL();
	_screenSpaceProperty.initializeGL();
	_performance.initializeGL();
	_help.initializeGL();
}

void GUI::deinitializeGL() {
	_program = nullptr;

    glDeleteVertexArrays(1, &vao);
	glDeleteBuffers(1, &vbo);

	_property.deinitializeGL();
	_iSWAproperty.deinitializeGL();
	_screenSpaceProperty.deinitializeGL();
	_performance.deinitializeGL();
	_help.deinitializeGL();
}

void GUI::startFrame(float deltaTime, const glm::vec2& windowSize,
                     const glm::vec2& mousePos,
                     uint32_t mouseButtonsPressed)
{
    
	ImGuiIO& io = ImGui::GetIO();
	io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
	io.DeltaTime = deltaTime;
#ifdef __APPLE__
    io.MousePos = ImVec2(mousePos.x * 2, mousePos.y * 2);
#else
    io.MousePos = ImVec2(mousePos.x, mousePos.y);
#endif
    io.MouseDown[0] = mouseButtonsPressed & (1 << 0);
	io.MouseDown[1] = mouseButtonsPressed & (1 << 1);

	ImGui::NewFrame();
}

void GUI::endFrame() {
	renderMainWindow();

	if (_property.isEnabled())
		_property.render();
	if (_iSWAproperty.isEnabled())
		_iSWAproperty.render();
	if (_screenSpaceProperty.isEnabled())
		_screenSpaceProperty.render();
	if (_performance.isEnabled())
		_performance.render();
	if (_help.isEnabled())
		_help.render();

	ImGui::Render();
}

bool GUI::mouseButtonCallback(MouseButton button, MouseAction action) {
//bool GUI::mouseButtonCallback(int key, int action) {
	ImGuiIO& io = ImGui::GetIO();
	bool consumeEvent = io.WantCaptureMouse;
	return consumeEvent;
}

bool GUI::mouseWheelCallback(double position) {
	ImGuiIO& io = ImGui::GetIO();
	bool consumeEvent = io.WantCaptureMouse;
	if (consumeEvent)
		io.MouseWheel = static_cast<float>(position);

	return consumeEvent;
}

bool GUI::keyCallback(Key key, KeyModifier modifier, KeyAction action) {
//bool GUI::keyCallback(int key, int action) {
	ImGuiIO& io = ImGui::GetIO();
	bool consumeEvent = io.WantCaptureKeyboard;
	if (consumeEvent) {
        int keyIndex = static_cast<int>(key);
        if (keyIndex < 0)
            LERROR("Pressed key of index '" << keyIndex << "' was negative");
        else {
            if (action == KeyAction::Press)
                io.KeysDown[keyIndex] = true;
            if (action == KeyAction::Release)
                io.KeysDown[keyIndex] = false;
        }
	}
	return consumeEvent;
}

bool GUI::charCallback(unsigned int character, KeyModifier modifier) {
	ImGuiIO& io = ImGui::GetIO();
	bool consumeEvent = io.WantCaptureKeyboard;

	if (consumeEvent)
		io.AddInputCharacter((unsigned short)character);

	return consumeEvent;
}

//void GUI::registerProperty(properties::Property* prop) {
//	using namespace properties;
//
//	std::string className = prop->className();
//
//	if (className == "BoolProperty")
//		_boolProperties.insert(prop);
//	else if (className == "IntProperty")
//		_intProperties.insert(prop);
//	else if (className == "FloatProperty")
//		_floatProperties.insert(prop);
//	else if (className == "StringProperty")
//		_stringProperties.insert(prop);
//	else if (className == "Vec2Property")
//		_vec2Properties.insert(prop);
//	else if (className == "Vec3Property")
//		_vec3Properties.insert(prop);
//	else if (className == "Vec4Property")
//		_vec4Properties.insert(prop);
//	else if (className == "OptionProperty")
//		_optionProperty.insert(prop);
//	else if (className == "TriggerProperty")
//		_triggerProperty.insert(prop);
//	else if (className == "SelectionProperty")
//		_selectionProperty.insert(prop);
//	else {
//		LWARNING("Class name '" << className << "' not handled in GUI generation");
//		return;
//	}
//
//	std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
//	size_t pos = fullyQualifiedId.find('.');
//	std::string owner = fullyQualifiedId.substr(0, pos);
//
//	auto it =_propertiesByOwner.find(owner);
//	if (it == _propertiesByOwner.end())
//		_propertiesByOwner[owner] = { prop };
//	else
//		it->second.push_back(prop);
//
//}

//>>>>>>> develop
void GUI::renderMainWindow() {
	ImGui::Begin("OpenSpace GUI", nullptr);

	ImGui::Checkbox("Scene Graph Properties", &_property._isEnabled);
	ImGui::Checkbox("ScreenSpace Properties", &_screenSpaceProperty._isEnabled);
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
	ImGui::Checkbox("iSWA Properties", &_iSWAproperty._isEnabled);
#endif
	ImGui::Checkbox("Performance", &_performance._isEnabled);
    _origin.render();
    _time.render();

    // These are temporary until the scalegraph is in effect ---abock
    bool toSun = ImGui::Button("Coordinate System to Sun");
    bool toPluto = ImGui::Button("Coordinate System to Pluto");
    bool toJupiter = ImGui::Button("Coordinate System to Jupiter");
    bool to67P = ImGui::Button("Coordinate System to 67P");

    if (toSun)
        OsEng.scriptEngine().queueScript("openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun');");
    if (toPluto)
        OsEng.scriptEngine().queueScript("openspace.setPropertyValue('Interaction.coordinateSystem', 'Pluto');");
    if (toJupiter)
        OsEng.scriptEngine().queueScript("openspace.setPropertyValue('Interaction.coordinateSystem', 'Jupiter');");
    if (to67P)
        OsEng.scriptEngine().queueScript("openspace.setPropertyValue('Interaction.coordinateSystem', '67P');");

	ImGui::Checkbox("Help", &_help._isEnabled);

	static const int addImageBufferSize = 256;
    static char addImageBuffer[addImageBufferSize];
	ImGui::InputText("addImage", addImageBuffer, addImageBufferSize);

	if(ImGui::SmallButton("Add Image")){
		addScreenSpaceRenderable(std::string(addImageBuffer));
	}

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
   	if(ISWAManager::isInitialized()){
		static const int addCygnetBufferSize = 256;
	    static char addCygnetBuffer[addCygnetBufferSize];
		ImGui::InputText("addCynget", addCygnetBuffer, addCygnetBufferSize);

		if(ImGui::SmallButton("Add Cygnet")){
			ISWAManager::ref().addISWACygnet(std::string(addCygnetBuffer));
		}
	}
#endif

	ImGui::End();
}

//<<<<<<< HEAD
namespace {
//=======
//void GUI::renderPropertyWindow() {
//	using namespace properties;
//
//	ImGui::Begin("Properties", &_showPropertyWindow, size, 0.5f);
//
//	//ImGui::ShowUserGuide();
//	ImGui::Spacing();
//
//	for (const auto& p : _propertiesByOwner) {
//		if (ImGui::CollapsingHeader(p.first.c_str())) {
//			for (properties::Property* prop : p.second) {
//				if (_boolProperties.find(prop) != _boolProperties.end()) {
//					renderBoolProperty(prop, p.first);
//					continue;
//				}
//
//				if (_intProperties.find(prop) != _intProperties.end()) {
//					renderIntProperty(prop, p.first);
//					continue;
//				}
//
//				if (_floatProperties.find(prop) != _floatProperties.end()) {
//					renderFloatProperty(prop, p.first);
//					continue;
//				}
//
//				if (_vec2Properties.find(prop) != _vec2Properties.end()) {
//					renderVec2Property(prop, p.first);
//					continue;
//				}
//
//				if (_vec3Properties.find(prop) != _vec3Properties.end()) {
//					renderVec3Property(prop, p.first);
//					continue;
//				}
//
//				if (_vec4Properties.find(prop) != _vec4Properties.end()) {
//					renderVec4Property(prop, p.first);
//					continue;
//				}
//
//				if (_optionProperty.find(prop) != _optionProperty.end()) {
//					renderOptionProperty(prop, p.first);
//					continue;
//				}
//
//				if (_triggerProperty.find(prop) != _triggerProperty.end()) {
//					renderTriggerProperty(prop, p.first);
//					continue;
//				}
//
//				if (_selectionProperty.find(prop) != _selectionProperty.end()) {
//					renderSelectionProperty(prop, p.first);
//					continue;
//				}
//			}
//		}
//	}
//
//	ImGui::End();
//}
//
//void GUI::renderPerformanceWindow() {
//	// Copy and paste from renderengine.cpp::storePerformanceMeasurements method
//	const int8_t Version = 0;
//	const int nValues = 250;
//	const int lengthName = 256;
//	const int maxValues = 50;
//
//	struct PerformanceLayout {
//		int8_t version;
//		int32_t nValuesPerEntry;
//		int32_t nEntries;
//		int32_t maxNameLength;
//		int32_t maxEntries;
//
//		struct PerformanceLayoutEntry {
//			char name[lengthName];
//			float renderTime[nValues];
//			float updateRenderable[nValues];
//			float updateEphemeris[nValues];
//
//			int32_t currentRenderTime;
//			int32_t currentUpdateRenderable;
//			int32_t currentUpdateEphemeris;
//		};
//
//		PerformanceLayoutEntry entries[maxValues];
//	};
//
//	ImGui::Begin("Performance", &_showPerformanceWindow);
//	if (OsEng.renderEngine()->doesPerformanceMeasurements() &&
//			ghoul::SharedMemory::exists(RenderEngine::PerformanceMeasurementSharedData))
//	{
//		ImGui::SliderFloat2("Min values, max Value", _minMaxValues, 0.f, 10000.f);
//		_minMaxValues[1] = std::max(_minMaxValues[0], _minMaxValues[1]);
//
//		if (!_performanceMemory)
//			_performanceMemory = new ghoul::SharedMemory(RenderEngine::PerformanceMeasurementSharedData);
//
//		PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(_performanceMemory->pointer());
//
//		for (int i = 0; i < layout->nEntries; ++i) {
//			const PerformanceLayout::PerformanceLayoutEntry& entry = layout->entries[i];
//
//			if (ImGui::CollapsingHeader(entry.name)) {
//				std::string updateEphemerisTime = std::to_string(entry.updateEphemeris[entry.currentUpdateEphemeris - 1]) + "us";
//				ImGui::PlotLines("UpdateEphemeris", &entry.updateEphemeris[0], layout->nValuesPerEntry, 0, updateEphemerisTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//
//				std::string updateRenderableTime = std::to_string(entry.updateRenderable[entry.currentUpdateRenderable - 1]) + "us";
//				ImGui::PlotLines("UpdateRender", &entry.updateRenderable[0], layout->nValuesPerEntry, 0, updateRenderableTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//				
//				std::string renderTime = std::to_string(entry.renderTime[entry.currentRenderTime - 1]) + "us";
//				ImGui::PlotLines("RenderTime", &entry.renderTime[0], layout->nValuesPerEntry, 0, renderTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//			}
//		}
//	}
//	else {
//		ImGui::TextWrapped("Performance monitoring is disabled. Enable with "
//						   "'openspace.setPerformanceMeasurement(true)'");
//	}
//
//	ImGui::End();
//}
//
//namespace  {
//>>>>>>> develop

/**
 * \ingroup LuaScripts
 * show():
 * Shows the GUI
 */
int show(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.gui().setEnabled(true);
	return 0;
}

/**
 * \ingroup LuaScripts
 * hide():
 * Hides the console
 */
int hide(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.gui().setEnabled(false);
	return 0;
}

/**
 * \ingroup LuaScripts
 * toggle():
 * Toggles the console
 */
int toggle(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.gui().setEnabled(!OsEng.gui().isEnabled());
	return 0;
}

}

scripting::ScriptEngine::LuaLibrary GUI::luaLibrary() {
	return {
		"gui",
		{
			{
				"show",
				&show,
				"",
				"Shows the console"
			},
			{
				"hide",
				&hide,
				"",
				"Hides the console"
			},
			{
				"toggle",
				&toggle,
				"",
				"Toggles the console"
			}
		}
	};
}


} // namespace gui
} // namespace openspace

//
//
//
//
//
//
//
//
//
//
//
//
//
//
//#include <openspace/gui/gui.h>
//
//#include <openspace/engine/openspaceengine.h>
//#include <openspace/properties/property.h>
//
//#include <ghoul/io/texture/texturereader.h>
//#include <ghoul/opengl/ghoul_gl.h>
//#include <ghoul/opengl/programobject.h>
//#include <ghoul/opengl/texture.h>
//#include <ghoul/opengl/textureunit.h>
//#include <ghoul/misc/sharedmemory.h>
//
//#include <openspace/properties/scalarproperty.h>
//#include <openspace/properties/vectorproperty.h>
//#include <openspace/properties/stringproperty.h>
//#include <openspace/properties/optionproperty.h>
//#include <openspace/properties/selectionproperty.h>
//#include <openspace/properties/triggerproperty.h>
//
//#include <ghoul/filesystem/filesystem.h>
//#include <ghoul/filesystem/cachemanager.h>
//
//#include <imgui.h>
//#include <sgct.h>
//#define STB_IMAGE_IMPLEMENTATION
//#include <stb_image.h>
//
//namespace {
//	const std::string _loggerCat = "GUI";
//	const std::string configurationFile = "imgui.ini";
//
//
//namespace openspace {
//	namespace gui {
//
//GUI::GUI()
//	: _isEnabled(false)
//	, _showPropertyWindow(false)
//	, _showHelp(false)
//	, _performanceMemory(nullptr)
//{
//	_minMaxValues[0] = 100.f;
//	_minMaxValues[1] = 1000.f;
//}
//
//GUI::~GUI() {
//	delete _performanceMemory;
//	ImGui::Shutdown();
//}
//
//bool GUI::isEnabled() const {
//	return _isEnabled;
//}
//
//void GUI::setEnabled(bool enabled) {
//	_isEnabled = enabled;
//}
//
//void GUI::initialize() {
//	std::string cachedFile;
//	FileSys.cacheManager()->getCachedFile(configurationFile, "", cachedFile, true);
//
//	char* buffer = new char[cachedFile.size() + 1];
//
//#ifdef WIN32
//	strcpy_s(buffer, cachedFile.size() + 1, cachedFile.c_str());
//#else
//	strcpy(buffer, cachedFile.c_str());
//#endif
//
//	ImGuiIO& io = ImGui::GetIO();
//	io.IniFilename = buffer;
//	//io.IniSavingRate = 5.f;
//	io.DeltaTime = 1.f / 60.f;
//	io.PixelCenterOffset = 0.5f;
//	io.KeyMap[ImGuiKey_Tab] = SGCT_KEY_TAB;                       // Keyboard mapping. ImGui will use those indices to peek into the io.KeyDown[] array.
//	io.KeyMap[ImGuiKey_LeftArrow] = SGCT_KEY_LEFT;
//	io.KeyMap[ImGuiKey_RightArrow] = SGCT_KEY_RIGHT;
//	io.KeyMap[ImGuiKey_UpArrow] = SGCT_KEY_UP;
//	io.KeyMap[ImGuiKey_DownArrow] = SGCT_KEY_DOWN;
//	io.KeyMap[ImGuiKey_Home] = SGCT_KEY_HOME;
//	io.KeyMap[ImGuiKey_End] = SGCT_KEY_END;
//	io.KeyMap[ImGuiKey_Delete] = SGCT_KEY_DELETE;
//	io.KeyMap[ImGuiKey_Backspace] = SGCT_KEY_BACKSPACE;
//	io.KeyMap[ImGuiKey_Enter] = SGCT_KEY_ENTER;
//	io.KeyMap[ImGuiKey_Escape] = SGCT_KEY_ESCAPE;
//	io.KeyMap[ImGuiKey_A] = SGCT_KEY_A;
//	io.KeyMap[ImGuiKey_C] = SGCT_KEY_C;
//	io.KeyMap[ImGuiKey_V] = SGCT_KEY_V;
//	io.KeyMap[ImGuiKey_X] = SGCT_KEY_X;
//	io.KeyMap[ImGuiKey_Y] = SGCT_KEY_Y;
//	io.KeyMap[ImGuiKey_Z] = SGCT_KEY_Z;
//
//	io.RenderDrawListsFn = ImImpl_RenderDrawLists;
//	//io.SetClipboardTextFn = ImImpl_SetClipboardTextFn; // @TODO implement? ---abock
//	//io.GetClipboardTextFn = ImImpl_GetClipboardTextFn; // @TODO implement? ---abock
//}
//
//void GUI::initializeGL() {
//	_program = ghoul::opengl::ProgramObject::Build("GUI",
//		"${SHADERS}/gui_vs.glsl", "${SHADERS}/gui_fs.glsl");
//		
//    positionLocation = glGetAttribLocation(*_program, "in_position");
//    uvLocation = glGetAttribLocation(*_program, "in_uv");
//    colorLocation = glGetAttribLocation(*_program, "in_color");
//
//    glGenTextures(1, &fontTex);
//    glBindTexture(GL_TEXTURE_2D, fontTex);
//    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
//    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
//    const void* png_data;
//    unsigned int png_size;
//    ImGui::GetDefaultFontData(NULL, NULL, &png_data, &png_size);
//    int tex_x, tex_y, tex_comp;
//    void* tex_data = stbi_load_from_memory((const unsigned char*)png_data, (int)png_size, &tex_x, &tex_y, &tex_comp, 0);
//    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_x, tex_y, 0, GL_RGBA, GL_UNSIGNED_BYTE, tex_data);
//    stbi_image_free(tex_data);
//
//    glGenBuffers(1, &vbo);
//    glBindBuffer(GL_ARRAY_BUFFER, vbo);
//    glBufferData(GL_ARRAY_BUFFER, vboMaxSize, NULL, GL_DYNAMIC_DRAW);
//
//    glGenVertexArrays(1, &vao);
//    glBindVertexArray(vao);
//    glBindBuffer(GL_ARRAY_BUFFER, vbo);
//    glEnableVertexAttribArray(positionLocation);
//    glEnableVertexAttribArray(uvLocation);
//    glEnableVertexAttribArray(colorLocation);
//
//    glVertexAttribPointer(positionLocation, 2, GL_FLOAT, GL_FALSE, sizeof(ImDrawVert), (GLvoid*) offsetof(ImDrawVert, pos));
//    glVertexAttribPointer(uvLocation, 2, GL_FLOAT, GL_FALSE, sizeof(ImDrawVert), (GLvoid*) offsetof(ImDrawVert, uv));
//    glVertexAttribPointer(colorLocation, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(ImDrawVert), (GLvoid*) offsetof(ImDrawVert, col));
//    glBindVertexArray(0);
//    glBindBuffer(GL_ARRAY_BUFFER, 0);
//}
//
//void GUI::deinitializeGL() {
//	if(_program)
//		delete _program;
//	_program = nullptr;
//
//	if (vao) glDeleteVertexArrays(1, &vao);
//    if (vbo) glDeleteBuffers(1, &vbo);
//}
//
//void GUI::startFrame(float deltaTime,
//				const glm::vec2& windowSize,
//				 const glm::vec2& mousePos,
//				 bool mouseButtonsPressed[2])
//{
//	ImGuiIO& io = ImGui::GetIO();
//	io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
//	io.DeltaTime = deltaTime;
//	io.MousePos = ImVec2(mousePos.x, mousePos.y);
//	io.MouseDown[0] = mouseButtonsPressed[0];
//	io.MouseDown[1] = mouseButtonsPressed[1];
//
//	ImGui::NewFrame();
//}
//
//void GUI::endFrame() {
//	static bool show = true;
//	//ImGui::ShowTestWindow(&show);
//	renderMainWindow();
//	if (_showPropertyWindow)
//		renderPropertyWindow();
//	if (_showPerformanceWindow)
//		renderPerformanceWindow();
//
//	ImGui::Render();
//}
//
//bool GUI::mouseButtonCallback(int key, int action) {
//	ImGuiIO& io = ImGui::GetIO();
//	bool consumeEvent = io.WantCaptureMouse;
//	return consumeEvent;
//}
//
//bool GUI::mouseWheelCallback(int position) {
//	ImGuiIO& io = ImGui::GetIO();
//	bool consumeEvent = io.WantCaptureMouse;
//	if (consumeEvent)
//		io.MouseWheel = static_cast<float>(position);
//
//	return consumeEvent;
//}
//
//bool GUI::keyCallback(int key, int action) {
//	ImGuiIO& io = ImGui::GetIO();
//	bool consumeEvent = io.WantCaptureKeyboard;
//	if (consumeEvent) {
//		if (action == SGCT_PRESS)
//			io.KeysDown[key] = true;
//		if (action == SGCT_RELEASE)
//			io.KeysDown[key] = false;
//	}
//	return consumeEvent;
//}
//
//bool GUI::charCallback(unsigned int character) {
//	ImGuiIO& io = ImGui::GetIO();
//	bool consumeEvent = io.WantCaptureKeyboard;
//
//	if (consumeEvent)
//		io.AddInputCharacter((unsigned short)character);
//
//	return consumeEvent;
//}
//
//void GUI::registerProperty(properties::Property* prop) {
//	using namespace openspace::properties;
//
//	std::string className = prop->className();
//
//	if (className == "BoolProperty")
//		_boolProperties.insert(prop);
//	else if (className == "IntProperty")
//		_intProperties.insert(prop);
//	else if (className == "FloatProperty")
//		_floatProperties.insert(prop);
//	else if (className == "StringProperty")
//		_stringProperties.insert(prop);
//	else if (className == "Vec2Property")
//		_vec2Properties.insert(prop);
//	else if (className == "Vec3Property")
//		_vec3Properties.insert(prop);
//	else if (className == "OptionProperty")
//		_optionProperty.insert(prop);
//	else if (className == "TriggerProperty")
//		_triggerProperty.insert(prop);
//	else if (className == "SelectionProperty")
//		_selectionProperty.insert(prop);
//	else {
//		LWARNING("Class name '" << className << "' not handled in GUI generation");
//		return;
//	}
//
//	std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
//	size_t pos = fullyQualifiedId.find('.');
//	std::string owner = fullyQualifiedId.substr(0, pos);
//
//	auto it =_propertiesByOwner.find(owner);
//	if (it == _propertiesByOwner.end())
//		_propertiesByOwner[owner] = { prop };
//	else
//		it->second.push_back(prop);
//
//}
//
//void GUI::renderMainWindow() {
//	ImGui::Begin("OpenSpace GUI", nullptr);
//
//	ImGui::Checkbox("Properties", &_showPropertyWindow);
//	ImGui::Checkbox("Performance", &_showPerformanceWindow);
//	ImGui::Checkbox("Help", &_showHelp);
//
//	if (_showHelp) {
//		ImGui::Separator();
//		ImGui::ShowUserGuide();
//		ImGui::ShowTestWindow();
//	}
//
//	ImGui::End();
//}
//
//void GUI::renderPropertyWindow() {
//	using namespace properties;
//
//	ImGui::Begin("Properties", &_showPropertyWindow, size, 0.5f);
//
//	//ImGui::ShowUserGuide();
//	ImGui::Spacing();
//
//	for (const auto& p : _propertiesByOwner) {
//		if (ImGui::CollapsingHeader(p.first.c_str())) {
//			for (properties::Property* prop : p.second) {
//				if (_boolProperties.find(prop) != _boolProperties.end()) {
//					renderBoolProperty(prop, p.first);
//					continue;
//				}
//
//				if (_intProperties.find(prop) != _intProperties.end()) {
//					renderIntProperty(prop, p.first);
//					continue;
//				}
//
//				if (_floatProperties.find(prop) != _floatProperties.end()) {
//					renderFloatProperty(prop, p.first);
//					continue;
//				}
//
//				if (_vec2Properties.find(prop) != _vec2Properties.end()) {
//					renderVec2Property(prop, p.first);
//					continue;
//				}
//
//				if (_vec3Properties.find(prop) != _vec3Properties.end()) {
//					renderVec3Property(prop, p.first);
//					continue;
//				}
//
//				if (_optionProperty.find(prop) != _optionProperty.end()) {
//					renderOptionProperty(prop, p.first);
//					continue;
//				}
//
//				if (_triggerProperty.find(prop) != _triggerProperty.end()) {
//					renderTriggerProperty(prop, p.first);
//					continue;
//				}
//
//				if (_selectionProperty.find(prop) != _selectionProperty.end()) {
//					renderSelectionProperty(prop, p.first);
//					continue;
//				}
//			}
//		}
//	}
//
//	ImGui::End();
//}
//
//void GUI::renderPerformanceWindow() {
//	// Copy and paste from renderengine.cpp::storePerformanceMeasurements method
//	const int8_t Version = 0;
//	const int nValues = 250;
//	const int lengthName = 256;
//	const int maxValues = 50;
//
//	struct PerformanceLayout {
//		int8_t version;
//		int32_t nValuesPerEntry;
//		int32_t nEntries;
//		int32_t maxNameLength;
//		int32_t maxEntries;
//
//		struct PerformanceLayoutEntry {
//			char name[lengthName];
//			float renderTime[nValues];
//			float updateRenderable[nValues];
//			float updateEphemeris[nValues];
//
//			int32_t currentRenderTime;
//			int32_t currentUpdateRenderable;
//			int32_t currentUpdateEphemeris;
//		};
//
//		PerformanceLayoutEntry entries[maxValues];
//	};
//
//	ImGui::Begin("Performance", &_showPerformanceWindow);
//	if (OsEng.renderEngine().doesPerformanceMeasurements() &&
//			ghoul::SharedMemory::exists(RenderEngine::PerformanceMeasurementSharedData))
//	{
//		ImGui::SliderFloat2("Min values, max Value", _minMaxValues, 0.f, 10000.f);
//		_minMaxValues[1] = std::max(_minMaxValues[0], _minMaxValues[1]);
//
//		if (!_performanceMemory)
//			_performanceMemory = new ghoul::SharedMemory(RenderEngine::PerformanceMeasurementSharedData);
//
//		PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(_performanceMemory->pointer());
//
//		for (int i = 0; i < layout->nEntries; ++i) {
//			const PerformanceLayout::PerformanceLayoutEntry& entry = layout->entries[i];
//
//			if (ImGui::CollapsingHeader(entry.name)) {
//				std::string updateEphemerisTime = std::to_string(entry.updateEphemeris[entry.currentUpdateEphemeris - 1]) + "us";
//				ImGui::PlotLines("UpdateEphemeris", &entry.updateEphemeris[0], layout->nValuesPerEntry, 0, updateEphemerisTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//
//				std::string updateRenderableTime = std::to_string(entry.updateRenderable[entry.currentUpdateRenderable - 1]) + "us";
//				ImGui::PlotLines("UpdateRender", &entry.updateRenderable[0], layout->nValuesPerEntry, 0, updateRenderableTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//				
//				std::string renderTime = std::to_string(entry.renderTime[entry.currentRenderTime - 1]) + "us";
//				ImGui::PlotLines("RenderTime", &entry.renderTime[0], layout->nValuesPerEntry, 0, renderTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
//			}
//		}
//	}
//	else {
//		ImGui::TextWrapped("Performance monitoring is disabled. Enable with "
//						   "'openspace.setPerformanceMeasurement(true)'");
//	}
//
//	ImGui::End();
//}
//
//
//} // namespace openspace
//} // namespace gui
