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

#include <modules/imgui/include/guipropertycomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <modules/imgui/include/renderproperties.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>

//#define Debugging_ImGui_TreeNode_Indices

namespace {
    const ImVec2 Size = ImVec2(350, 500);

    constexpr openspace::properties::Property::PropertyInfo UseTreeInfo = {
        "TreeLayout",
        "Use Tree Layout",
        "If this value is checked, this component will display the properties using a "
        "tree layout, rather than using a flat map. This value should only be set on "
        "property windows that display SceneGraphNodes, or the application might crash.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OrderingInfo = {
        "Ordering",
        "Tree Ordering",
        "This list determines the order of the first tree layer if it is used. Elements "
        "present in this list will be shown first, with an alphabetical ordering for "
        "elements not listed.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    int nVisibleProperties(const std::vector<openspace::properties::Property*>& props)
    {
        using namespace openspace;
        using Visibility = properties::Property::Visibility;
        const Visibility visibilityFilter = global::openSpaceEngine->visibility();

        return static_cast<int>(std::count_if(
            props.begin(),
            props.end(),
            [visibilityFilter](properties::Property* p) {
                using V = properties::Property::Visibility;
                return static_cast<std::underlying_type_t<V>>(visibilityFilter) >=
                       static_cast<std::underlying_type_t<V>>(p->visibility());
            }
        ));
    }

    void renderTooltip(openspace::properties::PropertyOwner* propOwner) {
        const bool shouldDisplay = ImGui::IsItemHovered() &&
                                   (!propOwner->description().empty());

        if (shouldDisplay) {
            ImGui::SetTooltip("%s", propOwner->description().c_str());
        }
    }

    struct TreeNode {
        explicit TreeNode(std::string p)
            : path(std::move(p))
#ifdef Debugging_ImGui_TreeNode_Indices
            , index(nextIndex++)
#endif // Debugging_ImGui_TreeNode_Indices
        {}

        std::string path;
        std::vector<std::unique_ptr<TreeNode>> children;
        std::vector<openspace::SceneGraphNode*> nodes;
#ifdef Debugging_ImGui_TreeNode_Indices
        int index = 0;
        static int nextIndex;
#endif // Debugging_ImGui_TreeNode_Indices

    };

#ifdef Debugging_ImGui_TreeNode_Indices

    int TreeNode::nextIndex = 0;

#endif // Debugging_ImGui_TreeNode_Indices

    void addPathToTree(TreeNode& node, const std::vector<std::string>& path,
                       openspace::SceneGraphNode* owner)
    {
        if (path.empty()) {
            // No more path, so we have reached a leaf
            node.nodes.push_back(owner);
            return;
        }

        // Check if any of the children's paths contains the first part of the path
        const auto it = std::find_if(
            node.children.begin(),
            node.children.end(),
            [p = *path.begin()](const std::unique_ptr<TreeNode>& c) {
                return c->path == p;
            }
        );

        TreeNode* n = nullptr;
        if (it != node.children.end()) {
            // We have a child, so we use it
            n = it->get();
        }
        else {
            // We don't have a child, so we must generate it
            auto newNode = std::make_unique<TreeNode>(*path.begin());
            n = newNode.get();
            node.children.push_back(std::move(newNode));
        }

        // Recurse into the tree and chop off the first path
        addPathToTree(
            *n,
            std::vector<std::string>(path.begin() + 1, path.end()),
            owner
        );
    }

    void simplifyTree(TreeNode& node) {
        // Merging consecutive nodes if they only have a single child

        for (const std::unique_ptr<TreeNode>& c : node.children) {
            simplifyTree(*c);
        }

        if ((node.children.size() == 1) && (node.nodes.empty())) {
            node.path = node.path + "/" + node.children[0]->path;
            node.nodes = std::move(node.children[0]->nodes);
            std::vector<std::unique_ptr<TreeNode>> children = std::move(
                node.children[0]->children
            );
            node.children = std::move(children);
        }
    }

    void renderTree(const TreeNode& node,
            const std::function<void (openspace::properties::PropertyOwner*)>& renderFunc)
    {
        if (node.path.empty() || ImGui::TreeNode(node.path.c_str())) {
            for (const std::unique_ptr<TreeNode>& c : node.children) {
                renderTree(*c, renderFunc);
            }

            for (openspace::SceneGraphNode* n : node.nodes) {
                renderFunc(n);
            }

            if (!node.path.empty()) {
                ImGui::TreePop();
            }
        }
    }
} // namespace

namespace openspace::gui {

GuiPropertyComponent::GuiPropertyComponent(std::string identifier, std::string guiName,
                                           UseTreeLayout useTree)
    : GuiComponent(std::move(identifier), std::move(guiName))
    , _useTreeLayout(UseTreeInfo, useTree)
    , _treeOrdering(OrderingInfo)
{
    addProperty(_useTreeLayout);
    addProperty(_treeOrdering);
}

void GuiPropertyComponent::setPropertyOwners(
                                   std::vector<properties::PropertyOwner*> propertyOwners)
{
    _propertyOwners = std::move(propertyOwners);
}

void GuiPropertyComponent::setPropertyOwnerFunction(
                            std::function<std::vector<properties::PropertyOwner*>()> func)
{
    _propertyOwnerFunction = std::move(func);
}

void GuiPropertyComponent::renderPropertyOwner(properties::PropertyOwner* owner) {
    using namespace properties;

    if (owner->propertiesRecursive().empty()) {
        return;
    }

    const int nThisProperty = nVisibleProperties(owner->properties());
    ImGui::PushID(owner->identifier().c_str());
    const std::vector<PropertyOwner*>& subOwners = owner->propertySubOwners();
    for (PropertyOwner* subOwner : subOwners) {
        const std::vector<Property*>& properties = subOwner->propertiesRecursive();
        const int count = nVisibleProperties(properties);
        if (count == 0) {
            continue;
        }
        if (subOwners.size() == 1 && (nThisProperty == 0)) {
            renderPropertyOwner(subOwner);
        }
        else {
            const bool opened = ImGui::TreeNode(subOwner->guiName().c_str());
            renderTooltip(subOwner);
            if (opened) {
                renderPropertyOwner(subOwner);
                ImGui::TreePop();
            }
        }
    }

    if (!subOwners.empty()) {
        ImGui::Spacing();
    }

    std::map<std::string, std::vector<Property*>> propertiesByGroup;
    std::vector<Property*> remainingProperies;

    for (properties::Property* p : owner->properties()) {
        const std::string& group = p->groupIdentifier();
        if (group.empty()) {
            remainingProperies.push_back(p);
        }
        else {
            propertiesByGroup[group].push_back(p);
        }
    }

    using K = std::string;
    using V = std::vector<Property*>;
    for (const std::pair<const K, V>& p : propertiesByGroup) {
        const std::string& groupName = owner->propertyGroupName(p.first);
        if (ImGui::TreeNode(groupName.c_str())) {
            for (properties::Property* prop : p.second) {
                renderProperty(prop, owner);
            }
            ImGui::TreePop();
        }
    }

    if (!propertiesByGroup.empty()) {
        ImGui::Spacing();
    }

    for (properties::Property* prop : remainingProperies) {
        renderProperty(prop, owner);
    }
    ImGui::PopID();
}

void GuiPropertyComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::SetNextWindowSize(Size, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.75f);
    ImGui::Begin(guiName().c_str(), &v);
    _isEnabled = v;

    _isCollapsed = ImGui::IsWindowCollapsed();
    using namespace properties;

    std::vector<properties::PropertyOwner*> owners =
        _propertyOwnerFunction ? _propertyOwnerFunction() : _propertyOwners;

    std::sort(
        owners.begin(),
        owners.end(),
        [](properties::PropertyOwner* lhs, properties::PropertyOwner* rhs) {
            return lhs->guiName() < rhs->guiName();
        }
    );

    if (_useTreeLayout) {
        for (properties::PropertyOwner* owner : owners) {
            ghoul_assert(
                dynamic_cast<SceneGraphNode*>(owner),
                "When using the tree layout, all owners must be SceneGraphNodes"
            );
            (void)owner; // using [[maybe_unused]] in the for loop gives an error
        }

        // Sort:
        // if guigrouping, sort by name and shortest first, but respect the user specified
        // ordering then all w/o guigroup
        const std::vector<std::string>& ordering = _treeOrdering;
        std::stable_sort(
            owners.begin(),
            owners.end(),
            [&ordering](PropertyOwner* lhs, PropertyOwner* rhs) {
                const std::string lhsGrp = dynamic_cast<SceneGraphNode*>(lhs)->guiPath();
                const std::string rhsGrp = dynamic_cast<SceneGraphNode*>(rhs)->guiPath();

                if (lhsGrp.empty()) {
                    return false;
                }
                if (rhsGrp.empty()) {
                    return true;
                }

                if (ordering.empty()) {
                    return lhsGrp < rhsGrp;
                }

                std::vector<std::string> lhsToken = ghoul::tokenizeString(lhsGrp, '/');
                // The first token is always empty
                auto lhsIt = std::find(ordering.begin(), ordering.end(), lhsToken[1]);

                std::vector<std::string> rhsToken = ghoul::tokenizeString(rhsGrp, '/');
                // The first token is always empty
                auto rhsIt = std::find(ordering.begin(), ordering.end(), rhsToken[1]);

                if (lhsIt != ordering.end() && rhsIt != ordering.end()) {
                    if (lhsToken[1] != rhsToken[1]) {
                        // If both top-level groups are in the ordering list, the
                        // order of the iterators gives us the order of the groups
                        return lhsIt < rhsIt;
                    }
                    else {
                        return lhsGrp < rhsGrp;
                    }
                }
                else if (lhsIt != ordering.end() && rhsIt == ordering.end()) {
                    // If only one of them is in the list, we have a sorting
                    return true;
                }
                else if (lhsIt == ordering.end() && rhsIt != ordering.end()) {
                    return false;
                }
                else {
                    return lhsGrp < rhsGrp;
                }
            }
        );
    }

    // If the owners list is empty, we wnat to do the normal thing (-> nothing)
    // Otherwise, check if the first owner has a GUI group
    // This makes the assumption that the tree layout is only used if the owners are
    // SceenGraphNodes (checked above)
    const bool noGuiGroups = owners.empty() ||
                             (dynamic_cast<SceneGraphNode*>(*owners.begin()) &&
                       dynamic_cast<SceneGraphNode*>(*owners.begin())->guiPath().empty());

    auto renderProp = [this, owners](properties::PropertyOwner* pOwner) {
        const int count = nVisibleProperties(pOwner->propertiesRecursive());

        if (count == 0) {
            return;
        }

        auto header = [&owners, &pOwner]() -> bool {
            if (owners.size() > 1) {
                // Create a header in case we have multiple owners
                return ImGui::CollapsingHeader(pOwner->guiName().c_str());
            }
            else if (!pOwner->identifier().empty()) {
                // If the owner has a name, print it first
                ImGui::Text("%s", pOwner->guiName().c_str());
                ImGui::Spacing();
                return true;
            }
            else {
                // Otherwise, do nothing
                return true;
            }
        };

        if (header()) {
            renderPropertyOwner(pOwner);
        }
    };

    if (!_useTreeLayout || noGuiGroups) {
        std::for_each(owners.begin(), owners.end(), renderProp);
    }
    else { // _useTreeLayout && gui groups exist
        TreeNode root("");

        for (properties::PropertyOwner* pOwner : owners) {
            // We checked above that pOwner is a SceneGraphNode
            SceneGraphNode* nOwner = static_cast<SceneGraphNode*>(pOwner);
            const std::string gui = nOwner->guiPath();
            if (gui.empty()) {
                // We know that we are done now since we stable_sort:ed them above
                break;
            }
            const std::vector<std::string> paths =
                ghoul::tokenizeString(gui.substr(1), '/');
            addPathToTree(root, paths, nOwner);
        }

        simplifyTree(root);

        renderTree(root, renderProp);

        ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

        for (properties::PropertyOwner* pOwner : owners) {
            // We checked above that pOwner is a SceneGraphNode
            SceneGraphNode* nOwner = static_cast<SceneGraphNode*>(pOwner);

            if (!nOwner->guiPath().empty()) {
                continue;
            }

            renderProp(pOwner);
        }
    }

    ImGui::End();
}

void GuiPropertyComponent::renderProperty(properties::Property* prop,
                                          properties::PropertyOwner* owner)
{
    using Func = std::function<
        void(properties::Property*, const std::string&, ShowToolTip, double)
    >;
    static const std::map<std::string, Func> FunctionMapping = {
        { "BoolProperty", &renderBoolProperty },
        { "DoubleProperty", &renderDoubleProperty },
        { "IntProperty", &renderIntProperty },
        { "IVec2Property", &renderIVec2Property },
        { "IVec3Property", &renderIVec3Property },
        { "IVec4Property", &renderIVec4Property },
        { "FloatProperty", &renderFloatProperty },
        { "Vec2Property", &renderVec2Property },
        { "Vec3Property", &renderVec3Property },
        { "Vec4Property", &renderVec4Property },
        { "DVec2Property", &renderDVec2Property },
        { "DVec3Property", &renderDVec3Property },
        { "DVec4Property", &renderDVec4Property },
        { "DMat2Property", &renderDMat2Property },
        { "DMat3Property", &renderDMat3Property },
        { "DMat4Property", &renderDMat4Property },
        { "StringProperty", &renderStringProperty },
        { "DoubleListProperty", &renderDoubleListProperty },
        { "IntListProperty", &renderIntListProperty },
        { "StringListProperty", &renderStringListProperty },
        { "OptionProperty", &renderOptionProperty },
        { "TriggerProperty", &renderTriggerProperty },
        { "SelectionProperty", &renderSelectionProperty }
    };

    // Check if the visibility of the property is high enough to be displayed
    using V = properties::Property::Visibility;
    using Visibility = openspace::properties::Property::Visibility;
    const Visibility visibilityFilter = openspace::global::openSpaceEngine->visibility();
    const auto v = static_cast<std::underlying_type_t<V>>(visibilityFilter);
    const auto propV = static_cast<std::underlying_type_t<V>>(prop->visibility());
    if (v >= propV) {
        auto it = FunctionMapping.find(std::string(prop->className()));
        if (it != FunctionMapping.end()) {
            if (owner) {
                it->second(
                    prop,
                    owner->identifier(),
                    ShowToolTip(_showHelpTooltip),
                    _tooltipDelay
                );
            }
            else {
                it->second(
                    prop,
                    "",
                    ShowToolTip(_showHelpTooltip),
                    _tooltipDelay
                );
            }
        }
    }
}

} // namespace openspace::gui
