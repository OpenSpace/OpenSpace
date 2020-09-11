#ifndef OSMODULES_H
#define OSMODULES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

QT_BEGIN_NAMESPACE
namespace Ui {
class osmodules;
}
QT_END_NAMESPACE

struct Module {
    std::string name;
    std::string loadedInstruction;
    std::string notLoadedInstruction;
};

class osmodules : public QDialog
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
    explicit osmodules(std::vector<Module>& imported, QWidget *parent = nullptr);
    ~osmodules();

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(Module m);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);

    Ui::osmodules *ui;
    QWidget* _parent;
    std::vector<Module>& _imported;
    std::vector<Module> _data;
    std::vector<QListWidgetItem*> _modulesListItems;
    bool _editModeNewItem = false;
};

#endif // OSMODULES_H
