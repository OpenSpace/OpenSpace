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

#include <openspace/interaction/touchbar.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/fmt.h>

using namespace openspace;

// Apple uses 'defer' as named arguments in some functions, so unfortunately, we have to
// undef our defer macro from ghoul/misc/defer.h
#undef defer
#import <AppKit/NSButton.h>
#import <AppKit/NSCustomTouchBarItem.h>
#import <AppKit/NSTouchBar.h>
#import <AppKit/NSWindow.h>
#import <Foundation/Foundation.h>

static NSString* pauseResultId = @"com.openspaceproject.pause_resume";
static NSString* hideGuiId = @"com.openspaceproject.hide_gui";
static NSString* hideOnScreenTextId = @"com.openspaceproject.hide_onscreen";
NSArray* focusIdentifiers;


@interface TouchBarDelegate : NSObject <NSTouchBarDelegate>
    - (NSTouchBar *)makeTouchBar;
    - (NSTouchBarItem *)touchBar:(NSTouchBar *)touchBar
        makeItemForIdentifier:(NSTouchBarItemIdentifier)identifier;
    - (void)pauseResumeButtonAction:(id)sender;
    - (void)focusObjectAction:(id)sender;
    - (void)hideTextAction:(id)sender;
    - (void)hideGuiAction:(id)sender;
@end

@implementation TouchBarDelegate
    - (NSTouchBar *)makeTouchBar {
        NSTouchBar* touchBar = [[NSTouchBar alloc] init];
        touchBar.delegate = self;

        touchBar.customizationIdentifier = @"com.openspaceproject.main_touch_bar";

        NSArray* objs = [@[hideGuiId, hideOnScreenTextId,
            NSTouchBarItemIdentifierFixedSpaceSmall, pauseResultId,
            NSTouchBarItemIdentifierFlexibleSpace]
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
        // Remove the unused variable warning
        (void)touchBar;

        if ([identifier isEqualToString:pauseResultId]) {
            NSButton* button = [NSButton
                buttonWithTitle:NSLocalizedString(
                    (global::timeManager->isPaused() ? @"Resume" : @"Pause"),
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

            return [touchBarItem autorelease];
        }

        if ([identifier isEqualToString:hideOnScreenTextId]) {
            NSButton* button = [NSButton
                buttonWithTitle:@"Toggle Text"
                target:self action:@selector(hideTextAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:hideOnScreenTextId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Toogles on-screen text",
                @""
            );

            return [touchBarItem autorelease];
        }

        if ([identifier isEqualToString:hideGuiId]) {
            NSButton* button = [NSButton
                buttonWithTitle:@"Toggle GUI"
                target:self action:@selector(hideGuiAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:hideGuiId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Toggles the main GUI",
                @""
            );

            return [touchBarItem autorelease];
        }

        if ([focusIdentifiers containsObject:identifier]) {
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

            return [touchBarItem autorelease];
        }

        return nil;
    }

    - (void)pauseResumeButtonAction:(id)sender {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.togglePause();",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );

        NSButton* button = static_cast<NSButton*>(sender);
        // This check is inverted since the togglePause script has not run yet
        [button setTitle: global::timeManager->isPaused() ? @"Pause" : @"Resume"];
    }

    - (void)focusObjectAction:(id)sender {
        NSButton* button = static_cast<NSButton*>(sender);

        NSString* title = [button title];

        std::string str = std::format(
            "openspace.setPropertyValueSingle('{}', '{}');\
             openspace.setPropertyValueSingle('{}', '');\
             openspace.setPropertyValueSingle('{}', '');",
             "NavigationHandler.OrbitalNavigator.Anchor", std::string([title UTF8String]),
             "NavigationHandler.OrbitalNavigator.Aim",
             "NavigationHandler.OrbitalNavigator.RetargetAnchor"
        );
        global::scriptEngine->queueScript(
            str,
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

    - (void)hideTextAction:(id)sender {
        // Remove unused variable warning
        (void)sender;

        global::scriptEngine->queueScript(
            "local isEnabled = openspace.propertyValue('Dashboard.IsEnabled');\
             openspace.setPropertyValueSingle('Dashboard.IsEnabled', not isEnabled);\
             openspace.setPropertyValueSingle('RenderEngine.ShowLog', not isEnabled);\
             openspace.setPropertyValueSingle('RenderEngine.ShowVersion', not isEnabled);\
             openspace.setPropertyValueSingle('RenderEngine.ShowCamera', not isEnabled)",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }

    - (void)hideGuiAction:(id)sender {
        // Remove unused variable warning
        (void)sender;
        global::scriptEngine->queueScript(
            "local isEnabled = openspace.propertyValue('Modules.CefWebGui.Visible');\
             openspace.setPropertyValueSingle('Modules.CefWebGui.Visible', not isEnabled);",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
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

    std::vector<SceneGraphNode*> ns = global::renderEngine->scene()->allSceneGraphNodes();

    std::sort(
        ns.begin(),
        ns.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->guiName() < rhs->guiName();
        }
    );

    NSMutableArray* ids = [[NSMutableArray alloc] init];
    for (SceneGraphNode* n : ns) {
        const std::vector<std::string>& tags = n->tags();
        auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            [ids addObject: [NSString stringWithCString:n->identifier().c_str()
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
