/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
static NSString* showFullGuiId = @"com.openspaceproject.show_full_gui";
static NSString* showSimpleGuiId = @"com.openspaceproject.show_simple_gui";
NSArray* focusIdentifiers;


@interface TouchBarDelegate : NSObject <NSTouchBarDelegate>
    - (NSTouchBar *)makeTouchBar;
    - (NSTouchBarItem *)touchBar:(NSTouchBar *)touchBar
        makeItemForIdentifier:(NSTouchBarItemIdentifier)identifier;
    - (void)pauseResumeButtonAction:(id)sender;
    - (void)focusObjectAction:(id)sender;
    - (void)fullGuiButtonAction:(id)sender;
    - (void)simpleGuiButtonAction:(id)sender;
@end

@implementation TouchBarDelegate
    - (NSTouchBar *)makeTouchBar {
        NSTouchBar* touchBar = [[NSTouchBar alloc] init];
        touchBar.delegate = self;

        touchBar.customizationIdentifier = @"com.openspaceproject.main_touch_bar";

        NSArray* objs = [@[showSimpleGuiId, showFullGuiId,
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
        // @TODO(abock):  Potential memory leak here by returning an alloc?

        // Remove the unused variable warning
        (void)touchBar;

        if ([identifier isEqualToString:pauseResultId]) {
            NSButton* button = [NSButton 
                buttonWithTitle:NSLocalizedString(
                    (global::timeManager.isPaused() ? @"Resume" : @"Pause"),
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

        if ([identifier isEqualToString:showFullGuiId]) {
            NSButton* button = [NSButton 
                buttonWithTitle:@"Full GUI"
                target:self action:@selector(fullGuiButtonAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:showFullGuiId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Toggles the full GUI",
                @""
            );

            return [touchBarItem autorelease];
        }

        if ([identifier isEqualToString:showSimpleGuiId]) {
            NSButton* button = [NSButton
                buttonWithTitle:@"Simple GUI"
                target:self action:@selector(simpleGuiButtonAction:)
            ];

            NSCustomTouchBarItem* touchBarItem = [
                [NSCustomTouchBarItem alloc]
                initWithIdentifier:showSimpleGuiId
            ];
            touchBarItem.view = button;
            touchBarItem.customizationLabel = NSLocalizedString(
                @"Toggles the simple GUI",
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
        global::scriptEngine.queueScript(
            "openspace.time.togglePause();",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        NSButton* button = static_cast<NSButton*>(sender);
        // This check is inverted since the togglePause script has not run yet
        [button setTitle: global::timeManager.isPaused() ? @"Pause" : @"Resume"];
    }

    - (void)focusObjectAction:(id)sender {
        NSButton* button = static_cast<NSButton*>(sender);

        NSString* title = [button title];

        global::scriptEngine.queueScript(
            "openspace.setPropertyValue('NavigationHandler.Origin', '" +
            std::string([title UTF8String]) + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    - (void)fullGuiButtonAction:(id)sender {
        // Remove unused variable warning
        (void)sender;
        global::scriptEngine.queueScript(
            "local b = openspace.getPropertyValue(\
                'Modules.ImGUI.Main.Enabled'\
            );\
            openspace.setPropertyValueSingle(\
                'Modules.ImGUI.Main.Enabled',\
                not b\
            );\
            openspace.setPropertyValueSingle(\
                'Modules.ImGUI.Main.IsHidden',\
                b\
            );",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }

    - (void)simpleGuiButtonAction:(id)sender {
        // Remove unused variable warning
        (void)sender;
        global::scriptEngine.queueScript(
"local b = openspace.getPropertyValue('Modules.ImGUI.Main.FeaturedProperties.Enabled');\n\
local c = openspace.getPropertyValue('Modules.ImGUI.Main.IsHidden');\n\
openspace.setPropertyValue('Modules.ImGUI.*.Enabled', false);\n\
if b and c then\n\
    -- This can happen if the main properties window is enabled, the main gui\n\
    -- is enabled and then closed again. So the main properties window is\n\
    -- enabled, but also all windows are hidden\n\
    openspace.setPropertyValueSingle('Modules.ImGUI.Main.IsHidden', false);\n\
    openspace.setPropertyValueSingle(\n\
        'Modules.ImGUI.Main.FeaturedProperties.Enabled',\n\
        true\n\
    );\n\
    openspace.setPropertyValueSingle(\n\
        'Modules.ImGUI.Main.SpaceTime.Enabled',\n\
        true\n\
    );\n\
else\n\
    openspace.setPropertyValueSingle(\n\
        'Modules.ImGUI.Main.FeaturedProperties.Enabled',\n\
        not b\n\
    );\n\
    openspace.setPropertyValueSingle(\n\
        'Modules.ImGUI.Main.SpaceTime.Enabled',\n\
        not b\n\
    );\n\
    openspace.setPropertyValueSingle('Modules.ImGUI.Main.IsHidden', b);\n\
end",
            scripting::ScriptEngine::RemoteScripting::No
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
        global::renderEngine.scene()->allSceneGraphNodes();

    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->guiName() < rhs->guiName();
        }
    );

    NSMutableArray* ids = [[NSMutableArray alloc] init];
    for (SceneGraphNode* n : nodes) {
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
