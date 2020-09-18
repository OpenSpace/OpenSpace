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
    explicit keybindings(openspace::Profile* imported, QWidget *parent = nullptr);
    ~keybindings();
    void keyPressEvent(QKeyEvent *evt);

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(openspace::Profile::Keybinding k);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    int indexInKeyMapping(std::vector<int>& mapVector, int keyInt);
    bool areRequiredFormsFilled();
    std::string truncateString(std::string& s);
    void replaceChars(std::string& src, const std::string& from,
        const std::string& to);

    Ui::keybindings *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    std::vector<openspace::Profile::Keybinding> _data;
    std::vector<QListWidgetItem*> _keybindingsListItems;
    std::vector<int> _mapModKeyComboBoxIndexToKeyValue;
    std::vector<int> _mapKeyComboBoxIndexToKeyValue;
    bool _editModeNewItem = false;
};

#endif // KEYBINDINGS_H
