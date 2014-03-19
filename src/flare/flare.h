/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef __FLARE_H__
#define __FLARE_H__

#include <GL/glew.h>
#include <sgct.h>
#include <flare/Animator.h>
#include <flare/Raycaster.h>
#include <flare/Config.h>

namespace openspace {

class Flare {
public:
	Flare();
	~Flare();

	void render();
	void initNavigation();

	void keyboard(int key, int action);
	void mouse(int button, int action);
	void preSync();
	void postDraw();
	void encode();
	void decode();

private:
	void init();

	osp::Config* _config;
	osp::Raycaster* _raycaster;
	osp::Animator* _animator;

	sgct::SharedBool _animationPaused;
	sgct::SharedBool _fpsMode;
	sgct::SharedFloat _elapsedTime;
	sgct::SharedInt _manualTimestep;
	sgct::SharedFloat _pitch;
	sgct::SharedFloat _yaw;
	sgct::SharedFloat _roll;
	sgct::SharedFloat _translateX;
	sgct::SharedFloat _translateY;
	sgct::SharedFloat _translateZ;
	sgct::SharedBool _reloadFlag;

	float _oldTime;
	float _currentTime;
	bool _leftMouseButton;
	double _currentMouseX;
	double _currentMouseY;
	double _lastMouseX;
	double _lastMouseY;
};

} // namespace openspace

#endif // __FLARE_H__
