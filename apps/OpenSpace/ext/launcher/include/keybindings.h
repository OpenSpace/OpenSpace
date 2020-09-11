#ifndef KEYBINDINGS_H
#define KEYBINDINGS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

QT_BEGIN_NAMESPACE
namespace Ui {
class keybindings;
}
QT_END_NAMESPACE

enum class Key {
    Unknown =           -1,
    Space =             32,
    Apostrophe =        39,
    Comma =             44,
    Minus =             45,
    Period =            46,
    Slash =             47,
    Num0 =              48,
    Num1 =              49,
    Num2 =              50,
    Num3 =              51,
    Num4 =              52,
    Num5 =              53,
    Num6 =              54,
    Num7 =              55,
    Num8 =              56,
    Num9 =              57,
    SemiColon =         59,
    Equal =             61,
    A =                 65,
    B =                 66,
    C =                 67,
    D =                 68,
    E =                 69,
    F =                 70,
    G =                 71,
    H =                 72,
    I =                 73,
    J =                 74,
    K =                 75,
    L =                 76,
    M =                 77,
    N =                 78,
    O =                 79,
    P =                 80,
    Q =                 81,
    R =                 82,
    S =                 83,
    T =                 84,
    U =                 85,
    V =                 86,
    W =                 87,
    X =                 88,
    Y =                 89,
    Z =                 90,
    LeftBracket =       91,
    BackSlash =         92,
    RightBracket =      93,
    GraveAccent =       96,
    World1 =            161,
    World2 =            162,
    Escape =            256,
    Enter =             257,
    Tab =               258,
    BackSpace =         259,
    Insert =            260,
    Delete =            261,
    Right =             262,
    Left =              263,
    Down =              264,
    Up =                265,
    PageUp =            266,
    PageDown =          267,
    Home =              268,
    End =               269,
    CapsLock =          280,
    ScrollLock =        281,
    NumLock =           282,
    PrintScreen =       283,
    Pause =             284,
    F1 =                290,
    F2 =                291,
    F3 =                292,
    F4 =                293,
    F5 =                294,
    F6 =                295,
    F7 =                296,
    F8 =                297,
    F9 =                298,
    F10 =               299,
    F11 =               300,
    F12 =               301,
    F13 =               302,
    F14 =               303,
    F15 =               304,
    F16 =               305,
    F17 =               306,
    F18 =               307,
    F19 =               308,
    F20 =               309,
    F21 =               310,
    F22 =               311,
    F23 =               312,
    F24 =               313,
    F25 =               314,
    Keypad0 =           320,
    Keypad1 =           321,
    Keypad2 =           322,
    Keypad3 =           323,
    Keypad4 =           324,
    Keypad5 =           325,
    Keypad6 =           326,
    Keypad7 =           327,
    Keypad8 =           328,
    Keypad9 =           329,
    KeypadDecimal =     330,
    KeypadDivide =      331,
    KeypadMultiply =    332,
    KeypadSubtract =    333,
    KeypadAdd =         334,
    KeypadEnter =       335,
    LeftShift =         340,
    LeftControl =       341,
    LeftAlt =           342,
    LeftSuper =         343,
    RightShift =        344,
    RightControl =      345,
    RightAlt =          346,
    RightSuper =        347,
    Menu =              348,
    Last =              Menu
};

static const std::map<int, std::string> keyNames = {
    {32, "Space"},
    {39, "'"},
    {44, ","},
    {45, "-"},
    {46, "."},
    {47, "/"},
    {48, "0"},
    {49, "1"},
    {50, "2"},
    {51, "3"},
    {52, "4"},
    {53, "5"},
    {54, "6"},
    {55, "7"},
    {56, "8"},
    {57, "9"},
    {59, ";"},
    {61, "="},
    {65, "A"},
    {66, "B"},
    {67, "C"},
    {68, "D"},
    {69, "E"},
    {70, "F"},
    {71, "G"},
    {72, "H"},
    {73, "I"},
    {74, "J"},
    {75, "K"},
    {76, "L"},
    {77, "M"},
    {78, "N"},
    {79, "O"},
    {80, "P"},
    {81, "Q"},
    {82, "R"},
    {83, "S"},
    {84, "T"},
    {85, "U"},
    {86, "V"},
    {87, "W"},
    {88, "X"},
    {89, "Y"},
    {90, "Z"},
    {91, "["},
    {92, "\\"},
    {93, "]"},
    {96, "`"},
    {161, "World1"},
    {162, "World2"},
    {256, "Escape"},
    {257, "Enter"},
    {258, "Tab"},
    {259, "BackSpace"},
    {260, "Insert"},
    {261, "Delete"},
    {262, "Right"},
    {263, "Left"},
    {264, "Down"},
    {265, "Up"},
    {266, "PageUp"},
    {267, "PageDown"},
    {268, "Home"},
    {269, "End"},
    {280, "CapsLock"},
    {281, "ScrollLock"},
    {282, "NumLock"},
    {283, "PrintScreen"},
    {284, "Pause"},
    {290, "F1"},
    {291, "F2"},
    {292, "F3"},
    {293, "F4"},
    {294, "F5"},
    {295, "F6"},
    {296, "F7"},
    {297, "F8"},
    {298, "F9"},
    {299, "F10"},
    {300, "F11"},
    {301, "F12"},
    {302, "F13"},
    {303, "F14"},
    {304, "F15"},
    {305, "F16"},
    {306, "F17"},
    {307, "F18"},
    {308, "F19"},
    {309, "F20"},
    {310, "F21"},
    {311, "F22"},
    {312, "F23"},
    {313, "F24"},
    {314, "F25"},
    {320, "Keypad 0"},
    {321, "Keypad 1"},
    {322, "Keypad 2"},
    {323, "Keypad 3"},
    {324, "Keypad 4"},
    {325, "Keypad 5"},
    {326, "Keypad 6"},
    {327, "Keypad 7"},
    {328, "Keypad 8"},
    {329, "Keypad 9"},
    {330, "Keypad ."},
    {331, "Keypad /"},
    {332, "Keypad *"},
    {333, "Keypad -"},
    {334, "Keypad +"},
    {335, "Keypad Enter"},
    {340, "Left Shift"},
    {341, "Left Control"},
    {342, "Left Alt"},
    {343, "Left Super"},
    {344, "Right Shift"},
    {345, "Right Control"},
    {346, "Right Alt"},
    {347, "Right Super"},
    {348, "Menu"}
};

enum class KeyModifier : int {
    NoModifier = 0,
    Shift = 0x0001,
    Control = 0x0002,
    Alt = 0x0004,
    Super = 0x0008,
    Last = Super
};

static const std::map<int, std::string> keyModifierNames = {
    {0, ""},
    {0x0001, "Shift"},
    {0x0002, "Control"},
    {0x0004, "Alt"},
    {0x0008, "Super"},
    {0x0003, "Shift+Control"},
    {0x0005, "Shift+Alt"},
    {0x0009, "Shift+Super"},
    {0x0006, "Control+Alt"},
    {0x000A, "Control+Super"},
    {0x000C, "Alt+Super"},
    {0x0007, "Shift+Control+Alt"},
    {0x000B, "Shift+Control+Super"},
    {0x000D, "Shift+Alt+Super"},
    {0x000E, "Control+Alt+Super"},
    {0x000F, "Shift+Control+Alt+Super"}
};

struct KeyWithModifier {
    Key key;
    KeyModifier modifier;
};

struct Keybinding {
    KeyWithModifier key;
    std::string documentation;
    std::string name;
    std::string guiPath;
    bool isLocal;
    std::string script;
};

class keybindings : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void listItemSave();
    void listItemCancelSave();
    void transitionToEditMode();
    void parseSelections();

public:
    explicit keybindings(std::vector<Keybinding>& imported, QWidget *parent = nullptr);
    ~keybindings();

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(Keybinding k);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    int indexInKeyMapping(std::vector<int>& mapVector, int keyInt);
    bool areRequiredFormsFilled();
    std::string truncateString(std::string& s);
    void replaceChars(std::string& src, const std::string& from,
        const std::string& to);

    Ui::keybindings *ui;
    QWidget* _parent;
    std::vector<Keybinding>& _imported;
    std::vector<Keybinding> _data;
    std::vector<QListWidgetItem*> _keybindingsListItems;
    std::vector<int> _mapModKeyComboBoxIndexToKeyValue;
    std::vector<int> _mapKeyComboBoxIndexToKeyValue;
    bool _editModeNewItem = false;
};

#endif // KEYBINDINGS_H
