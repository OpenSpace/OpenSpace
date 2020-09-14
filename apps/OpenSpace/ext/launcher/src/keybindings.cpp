#include <openspace/scene/profile.h>
#include <openspace/util/keys.h>
#include "keybindings.h"
#include "./ui_keybindings.h"
#include <qevent.h>
#include <algorithm>

keybindings::keybindings(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::keybindings)
    , _imported(imported)
    , _data(imported->keybindings())
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        _keybindingsListItems.push_back(new QListWidgetItem(createOneLineSummary(_data[i])));
        ui->list->addItem(_keybindingsListItems[i]);
    }

    QStringList comboModKeysStringList;
    int modIdx = 0;
    for (auto const& m : openspace::KeyModifierNames) {
        comboModKeysStringList += QString(m.second.c_str());
        _mapModKeyComboBoxIndexToKeyValue.push_back(modIdx++);
    }
    ui->combo_keyMod->addItems(comboModKeysStringList);

    QStringList comboKeysStringList;
    for (int i = 0; i < static_cast<int>(openspace::Key::Last); ++i) {
        if (openspace::KeyNames.find(i) != openspace::KeyNames.end()) {
            comboKeysStringList += QString(openspace::KeyNames.at(i).c_str());
            //Create map to relate key combo box to actual integer value defined in Key
            _mapKeyComboBoxIndexToKeyValue.push_back(i);
        }
    }
    ui->combo_key->addItems(comboKeysStringList);

    connect(ui->list, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(listItemAdded()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(listItemSave()));
    connect(ui->button_cancel, SIGNAL(clicked()), this, SLOT(listItemCancelSave()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(listItemRemove()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));

    transitionFromEditMode();
}

QString keybindings::createOneLineSummary(openspace::Profile::Keybinding k) {
    std::string summary;

    int keymod = static_cast<int>(k.key.modifier);
    if (keymod != static_cast<int>(openspace::KeyModifier::NoModifier)) {
        summary += openspace::KeyModifierNames.at(keymod) + "+";
    }
    int keyname = static_cast<int>(k.key.key);

    summary += openspace::KeyNames.at(keyname) + " : ";
    summary += truncateString(k.name) + " (";
    summary += truncateString(k.documentation) + ") @ ";
    summary += truncateString(k.guiPath) + " ";
    summary += (k.isLocal) ? "local" : "not local";
    summary += " `" + truncateString(k.script) + "`";

    return QString(summary.c_str());
}

std::string keybindings::truncateString(std::string& s) {
    const size_t maxLength = 50;
    replaceChars(s, "\n", ";");
    if (s.length() > maxLength) {
        s.resize(maxLength);
        s += "...";
    }
    return s;
}

void keybindings::replaceChars(std::string& src, const std::string& from,
                               const std::string& to)
{
    std::string newString;
    std::string::size_type found, last = 0;

    while((found = src.find(from, last)) != std::string::npos)
    {
        newString.append(src, last, (found - last));
        newString += to;
        last = found + from.length();
    }
    newString += src.substr(last);
    src.swap(newString);
}

void keybindings::listItemSelected(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    openspace::Profile::Keybinding& k = _data[index];
    ui->combo_keyMod->setCurrentIndex(
        indexInKeyMapping(_mapModKeyComboBoxIndexToKeyValue,
        static_cast<int>(k.key.modifier)));
    ui->combo_key->setCurrentIndex(
        indexInKeyMapping(_mapKeyComboBoxIndexToKeyValue,
        static_cast<int>(k.key.key)));

    //Do key here
    ui->line_name->setText(QString(k.name.c_str()));
    ui->line_documentation->setText(QString(k.documentation.c_str()));
    ui->line_guiPath->setText(QString(k.guiPath.c_str()));
    ui->text_script->setText(QString(k.script.c_str()));
    ui->checkBox_local->setChecked(k.isLocal);

    transitionToEditMode();
}

int keybindings::indexInKeyMapping(std::vector<int>& mapVector, int keyInt) {
    auto it = std::find (
        mapVector.begin(),
        mapVector.end(),
        keyInt
    );
    return std::distance(mapVector.begin(), it);
}

void keybindings::listItemAdded(void) {
    //Add new line at bottom of props list
    _data.push_back({
        {openspace::Key::Unknown, openspace::KeyModifier::NoModifier},
        "",
        "",
        "",
        true,
        ""
    });
    _keybindingsListItems.push_back(new QListWidgetItem("  (Enter details below and click 'Save')"));
    ui->list->addItem(_keybindingsListItems.back());

    //Scroll down to that blank line highlighted
    ui->list->setCurrentItem(_keybindingsListItems.back());

    //Blank-out the 2 text fields, set combo box to index 0
    ui->line_name->setText(QString(_data.back().name.c_str()));
    ui->line_documentation->setText(QString(_data.back().documentation.c_str()));
    ui->line_guiPath->setText(QString(_data.back().guiPath.c_str()));
    ui->text_script->setText(QString(_data.back().script.c_str()));

    ui->combo_keyMod->setCurrentIndex(static_cast<int>(_data.back().key.modifier));
    ui->combo_key->setCurrentIndex(static_cast<int>(_data.back().key.key));

    ui->checkBox_local->setChecked(false);
    _editModeNewItem = true;
}

void keybindings::listItemSave(void) {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    int keyModIdx = _mapModKeyComboBoxIndexToKeyValue.at(ui->combo_keyMod->currentIndex());
    _data[index].key.modifier = static_cast<openspace::KeyModifier>(keyModIdx);
    int keyIdx = _mapKeyComboBoxIndexToKeyValue.at(ui->combo_key->currentIndex());
    _data[index].key.key = static_cast<openspace::Key>(keyIdx);
    _data[index].name = ui->line_name->text().toUtf8().constData();
    _data[index].documentation = ui->line_documentation->text().toUtf8().constData();
    _data[index].guiPath = ui->line_guiPath->text().toUtf8().constData();
    _data[index].script = ui->text_script->toPlainText().toUtf8().constData();
    _data[index].isLocal = (ui->checkBox_local->isChecked());

    _keybindingsListItems.at(index)->setText(createOneLineSummary(_data[index]));
    transitionFromEditMode();
}

bool keybindings::areRequiredFormsFilled() {
    bool requiredFormsFilled = true;
    if (ui->combo_key->currentIndex() < 0) {
        ui->label_key->setText("<font color='red'>Key</font>");
        requiredFormsFilled = false;
    }
    else {
        ui->label_key->setText("<font color='black'>Key</font>");
    }
    if (ui->line_name->text().length() == 0) {
        ui->label_name->setText("<font color='red'>Name</font>");
        requiredFormsFilled = false;
    }
    else {
        ui->label_name->setText("<font color='black'>Name</font>");
    }
    if (ui->text_script->toPlainText().length() == 0) {
        ui->label_script->setText("<font color='red'>Script</font>");
        requiredFormsFilled = false;
    }
    else {
        ui->label_script->setText("<font color='black'>Script</font>");
    }
    return requiredFormsFilled;
}

void keybindings::listItemCancelSave(void) {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem) {
        if(_data.back().name.length() == 0 ||
           _data.back().script.length() == 0 ||
           _data.back().key.key == openspace::Key::Unknown)
        {
            listItemRemove();
        }
    }
    _editModeNewItem = false;
}

void keybindings::listItemRemove(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    ui->list->takeItem(index);
    _data.erase(_data.begin() + index);
    _keybindingsListItems.erase(_keybindingsListItems.begin() + index);
    transitionFromEditMode();
}

void keybindings::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);

    editBoxDisabled(false);
}

void keybindings::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);

    editBoxDisabled(true);
    ui->label_key->setText("<font color='black'>Key</font>");
    ui->label_name->setText("<font color='black'>Name</font>");
    ui->label_script->setText("<font color='black'>Script</font>");
}

void keybindings::editBoxDisabled(bool disabled) {
    ui->label_key->setDisabled(disabled);
    ui->combo_key->setDisabled(disabled);
    ui->combo_keyMod->setDisabled(disabled);
    ui->label_name->setDisabled(disabled);
    ui->line_name->setDisabled(disabled);
    ui->label_documentation->setDisabled(disabled);
    ui->line_documentation->setDisabled(disabled);
    ui->label_guiPath->setDisabled(disabled);
    ui->line_guiPath->setDisabled(disabled);
    ui->checkBox_local->setDisabled(disabled);
    ui->label_script->setDisabled(disabled);
    ui->text_script->setDisabled(disabled);
    ui->button_cancel->setDisabled(disabled);
    ui->button_save->setDisabled(disabled);
}

void keybindings::parseSelections() {
    _imported->setKeybindings(_data);
    accept();
}

keybindings::~keybindings() {
    for (auto p : _keybindingsListItems) {
        delete p;
    }
    delete ui;
}
