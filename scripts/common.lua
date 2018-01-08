helper = {}
helper.property = {}

-- Function that returns the string that inverts the fully qualified boolean property 'property'
helper.property.invert = function(prop)
    local escaped_property = "'" .. prop .. "'"
    return "openspace.setPropertyValue(" .. escaped_property .. ", not openspace.getPropertyValue(" .. escaped_property .. "));"
end

-- Function that returns the string that increments the 'property' by the 'value'
helper.property.increment = function(prop, value)
    local v = value or 1
    local escaped_property = "'" .. prop .. "'"
    return "openspace.setPropertyValue(" .. escaped_property .. ", openspace.getPropertyValue(" .. escaped_property .. ") + " .. v .. ");"
end

-- Function that returns the string that decrements the 'property' by the 'value'
helper.property.decrement = function(prop, value)
    return helper.property.increment(prop, -value)
end



helper.renderable = {}

-- Function that returns the string that enables/disables the renderable 'renderable'
helper.renderable.toggle = function(renderable)
    return helper.property.invert(renderable .. ".renderable.Enabled")
end

-- Function that returns the string that sets the enabled property of <renderable> to <enabled>
helper.renderable.setEnabled = function(renderable, enabled)
    return "openspace.setPropertyValue('" .. renderable .. ".renderable.Enabled', " .. (enabled and "true" or "false") .. ");";
end

helper.scheduledScript = {}
helper.scheduledScript.reversible = {}

-- Function that returns a lua table specifying a reversible ScheduledScript for 
-- setting the enabled property of <renderable> to <enabled> at time <time>.
helper.scheduledScript.reversible.setEnabled = function(time, renderable, enabled)
    return {
        Time = time,
        ForwardScript = helper.renderable.setEnabled(renderable, enabled),
        BackwardScript = helper.renderable.setEnabled(renderable, not enabled)
    }
end

helper.scheduledScript.setEnabled = function(time, renderable, enabled)
    return {
        Time = time,
        ForwardScript = helper.renderable.setEnabled(renderable, enabled)
    }
end