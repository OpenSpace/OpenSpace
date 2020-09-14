#ifndef PROPERTIES_H
#define PROPERTIES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

QT_BEGIN_NAMESPACE
namespace Ui {
class properties;
}
QT_END_NAMESPACE

class properties : public QDialog
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
    explicit properties(openspace::Profile* imported, QWidget *parent = nullptr);
    ~properties();

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(openspace::Profile::Property p);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    bool areRequiredFormsFilled();

    Ui::properties *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    std::vector<openspace::Profile::Property> _data;
    std::vector<QListWidgetItem*> _propListItems;
    bool _editModeNewItem = false;
};

#endif // PROPERTIES_H
