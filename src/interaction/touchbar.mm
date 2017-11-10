/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/interaction/touchbar.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>

#import <AppKit/NSButton.h>
#import <AppKit/NSCustomTouchBarItem.h>
#import <AppKit/NSTouchBar.h>
#import <AppKit/NSWindow.h>
#import <Foundation/Foundation.h>

static NSString* pauseResultId = @"com.openspaceproject.pause_resume";
static NSString* showGuiId = @"com.openspaceproject.show_gui";
NSArray* focusIdentifiers;


@interface TouchBarDelegate : NSObject <NSTouchBarDelegate>
    - (NSTouchBar *)makeTouchBar;
    - (NSTouchBarItem *)touchBar:(NSTouchBar *)touchBar
        makeItemForIdentifier:(NSTouchBarItemIdentifier)identifier;
    - (void)pauseResumeButtonAction:(id)sender;
    - (void)focusObjectAction:(id)sender;
    - (void)guiButtonAction:(id)sender;
@end

@implementation TouchBarDelegate
    - (NSTouchBar *)makeTouchBar {
        NSTouchBar* touchBar = [[NSTouchBar alloc] init];
        touchBar.delegate = self;

        touchBar.customizationIdentifier = @"com.openspaceproject.main_touch_bar";

        NSArray* objs = [@[showGuiId, NSTouchBarItemIdentifierFixedSpaceSmall,
            pauseResultId, NSTouchBarItemIdentifierFlexibleSpace]
            arrayByAddingObjectsFromArray: focusIdentifiers];

        // Set the default ordering of items.
        touchBar.defaultItemIdentifiers = objs;
            
        touchBar.customizationAllowedItemIdentifiers = objs;
        if ([focusIdentifiers count] > 0) {
            touchBar.principalItemIdentifier = [focusIdentifiers firstObject];
        }

        return touchBar;
    }

    - (NSTouchBarItem *)touchBar:(NSTouchBar *)touchBar
                        makeItemForIdentifier:(NSTouchBarItemIdentifier)identifier
    {
        if ([identifier isEqualToString:pauseResultId]) {
            NSButton* button = [NSButton 
                buttonWithTitle:NSLocalizedString(
                    (OsEng.timeManager().time().paused() ? @"Resume" : @"Pause"),
                    @""
                )
                target:self action:@selector(pauseResumeButtonAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:pauseResultId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Pauses / Resumes the in-game time",
                @""
            );

            return touchBarItem;
        }

        if ([identifier isEqualToString:showGuiId]) {
            NSButton* button = [NSButton 
                buttonWithTitle:@"GUI"
                target:self action:@selector(guiButtonAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:showGuiId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Pauses / Resumes the in-game time",
                @""
            );

            return touchBarItem;
        }

        if ([focusIdentifiers containsObject: identifier]) {
            NSButton* button = [NSButton
                buttonWithTitle:identifier
                target:self action:@selector(focusObjectAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:identifier
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                identifier,
                @""
            );

            return touchBarItem;
        }

        return nil;
    }

    - (void)pauseResumeButtonAction:(id)sender {
        OsEng.scriptEngine().queueScript(
            "openspace.time.togglePause();",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

        NSButton* button = (NSButton*)sender;
        // This check is inverted since the togglePause script has not run yet
        [button setTitle: OsEng.timeManager().time().paused() ? @"Pause" : @"Resume"];
    }

    - (void)focusObjectAction:(id)sender {
        NSButton* button = (NSButton*)sender;

        NSString* title = [button title];

        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('NavigationHandler.Origin', '" +
            std::string([title UTF8String]) + "');",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    - (void)guiButtonAction:(id)sender {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Global Properties.ImGUI.Main.Enabled', \
            not openspace.getPropertyValue('Global Properties.ImGUI.Main.Enabled'));",
            openspace::scripting::ScriptEngine::RemoteScripting::No
        );
    }
@end

namespace {
    TouchBarDelegate* g_TouchBarDelegate = nullptr;

} // namespace

namespace openspace {

void showTouchbar() {
    if (g_TouchBarDelegate) {
        [g_TouchBarDelegate dealloc];
        [focusIdentifiers dealloc];
    }
    else {
        g_TouchBarDelegate = [[TouchBarDelegate alloc] init];
        [NSApplication sharedApplication].automaticCustomizeTouchBarMenuItemEnabled = YES;
    }
    
    std::vector<SceneGraphNode*> nodes =
        OsEng.renderEngine().scene()->allSceneGraphNodes();

    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->name() < rhs->name();
        }
    );

    NSMutableArray* ids = [[NSMutableArray alloc] init];
    for (SceneGraphNode* n : nodes) {
        const std::vector<std::string>& tags = n->tags();
        auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            [ids addObject: [NSString stringWithCString:n->name().c_str()
                                      encoding:[NSString defaultCStringEncoding]]];
        }
    }

    focusIdentifiers = [ids copy];
    [ids dealloc];

    NSTouchBar* touchBar = [g_TouchBarDelegate makeTouchBar];

    // Attach the touch bar to all windows
    NSArray<NSWindow*>* windows = [NSApplication sharedApplication].windows;
    for (int i = 0; i < static_cast<int>(windows.count); ++i) {
        NSWindow* wnd = windows[i];
        wnd.touchBar = touchBar;
    }
}

} // namespace openspace
