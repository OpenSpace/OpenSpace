--[[ Commonly used OpenSpace configuration functions ]]--

helper = {}
helper.renderable = {}
helper.property = {}

-- These helpers are for scheduling lua scripts
-- See class ScriptScheduler and ScheduledScript for reference
helper.scheduledScript = {} 
helper.scheduledScript.reversible = {}

-- Function that sets the most common key bindings that are common to most (all?)
-- scenes
helper.setCommonKeys = function()
    openspace.bindKeyLocal("F1", "openspace.gui.toggle()")
    openspace.bindKeyLocal("F2", "openspace.setPerformanceMeasurement(true)")
    openspace.bindKeyLocal("F3", "openspace.setPerformanceMeasurement(false)")

    openspace.bindKeyLocal("t", "openspace.toggleFrametimeType(1)")
    openspace.bindKeyLocal("Shift+t", "openspace.toggleFrametimeType(0)")

    openspace.bindKeyLocal("ESC", "openspace.toggleShutdown()")

    openspace.bindKeyLocal("PRINT_SCREEN", "openspace.takeScreenshot()")
    openspace.bindKey("SPACE", "openspace.time.togglePause()")

    openspace.bindKey("COMMA", "openspace.setRenderer('Framebuffer');")
    openspace.bindKey("PERIOD", "openspace.setRenderer('ABuffer');")

    openspace.bindKey("f", helper.property.invert('Interaction.horizontalFriction'))
    openspace.bindKey("Shift+f", helper.property.invert('Interaction.verticalFriction'))
    openspace.bindKey("Ctrl+f", helper.property.invert('Interaction.rotationalFriction'))

    openspace.bindKey("w", "openspace.toggleFade(3)")
end

helper.setDeltaTimeKeys = function(t)
    local Keys = {
        '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
        'Shift+1', 'Shift+2', 'Shift+3', 'Shift+4', 'Shift+5', 'Shift+6', 'Shift+7', 'Shift+8', 'Shift+9', 'Shift+0',
        'Ctrl+1', 'Ctrl+2', 'Ctrl+3', 'Ctrl+4', 'Ctrl+5', 'Ctrl+6', 'Ctrl+7', 'Ctrl+8', 'Ctrl+9', 'Ctrl+0',
        'Alt+1', 'Alt+2', 'Alt+3', 'Alt+4', 'Alt+5', 'Alt+6', 'Alt+7', 'Alt+8', 'Alt+9', 'Alt+0'
    }

    if #t > #Keys then
        openspace.printError("Error settings delta time keys: Too many delta times (" .. #t .. ")")
        return
    end

    for i, v in ipairs(t) do
        openspace.bindKey(Keys[i], 'openspace.time.setDeltaTime(' .. v .. ")")
    end
end

-- Function that returns the string that inverts the fully qualified boolean property 'property'
helper.property.invert = function(property)
    local escaped_property = "'" .. property .. "'"
    return "openspace.setPropertyValue(" .. escaped_property .. ", not openspace.getPropertyValue(" .. escaped_property .. "));"
end

-- Function that returns the string that increments the 'property' by the 'value'
helper.property.increment = function(property, value)
    local v = value or 1
    local escaped_property = "'" .. property .. "'"
    return "openspace.setPropertyValue(" .. escaped_property .. ", openspace.getPropertyValue(" .. escaped_property .. ") + " .. v .. ")"
end

-- Function that returns the string that decrements the 'property' by the 'value'
helper.property.decrement = function(property, value)
    return helper.property.increment(property, -value)
end

-- Function that returns the string that enables/disables the renderable 'renderable'
helper.renderable.toggle = function(renderable)
    return helper.property.invert(renderable .. ".renderable.enabled")
end

-- Function that returns the string that sets the enabled property of <renderable> to <enabled>
helper.renderable.setEnabled = function(renderable, enabled)
    return "openspace.setPropertyValue('" .. renderable .. ".renderable.enabled', " .. (enabled and "true" or "false") .. ");";
end

-- Function that returns a lua table specifying a reversible ScheduledScript for 
-- setting the enabled property of <renderable> to <enabled> at time <time>.
helper.scheduledScript.reversible.setEnabled = function(time, renderable, enabled)
    return 
    {
        Time = time,
        ReversibleLuaScript = {
            Forward = helper.renderable.setEnabled(renderable, enabled),
            Backward = helper.renderable.setEnabled(renderable, not enabled)
        }
    }
end

helper.scheduledScript.setEnabled = function(time, renderable, enabled)
    return 
    {
        Time = time,
        ReversibleLuaScript = {
            Forward = helper.renderable.setEnabled(renderable, enabled),
            Backward = ""
        }
    }
end
