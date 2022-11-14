#pragma once

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include "../mol/util.h"

namespace openspace::properties {
    
struct RepresentationProperty : public PropertyOwner {
    properties::OptionProperty _type;
    properties::OptionProperty _color;
    properties::StringProperty _filter;
    properties::FloatProperty  _scale;

    RepresentationProperty(PropertyOwner::PropertyOwnerInfo info) :
        PropertyOwner(info),
            _type({"Type", "Type", "The Geometrical Representation of the Molecule"}),
            _color({"Coloring", "Coloring", "The Color function used to apply color to structures within the Molecule"}),
            _filter({"Filter", "Filter", "The filter expression controls which atoms the Representation should be applied to"})  ,
            _scale({"Scale", "Scale", "Scale for the Representation"})
        {
            _type.addOptions({
                { static_cast<int>(mol::rep::Type::SpaceFill), "Space Fill" },
                { static_cast<int>(mol::rep::Type::Licorice),  "Licorice" },
                { static_cast<int>(mol::rep::Type::Ribbons),   "Ribbons" },
                { static_cast<int>(mol::rep::Type::Cartoon),   "Cartoon" },
            });

            _color.addOptions({
                { static_cast<int>(mol::rep::Color::Cpk), "CPK" },
                { static_cast<int>(mol::rep::Color::AtomIndex), "Atom Index" },
                { static_cast<int>(mol::rep::Color::ResId), "Residue ID" },
                { static_cast<int>(mol::rep::Color::ResIndex), "Residue Index" },
                { static_cast<int>(mol::rep::Color::ChainId), "Chain ID" },
                { static_cast<int>(mol::rep::Color::ChainIndex), "Chain Index" },
                { static_cast<int>(mol::rep::Color::SecondaryStructure), "Secondary Structure" },
            });

            addProperty(_type);
            addProperty(_color);
            addProperty(_scale);
            addProperty(_filter);
        }
};

}
