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

struct Property {
    enum class SetType {
        SetPropertyValue,
        SetPropertyValueSingle
    };
    SetType setType;
    std::string name;
    std::string value;
};

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
    explicit properties(std::vector<Property>& imported, QWidget *parent = nullptr);
    ~properties();

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(Property p);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    bool areRequiredFormsFilled();

    Ui::properties *ui;
    QWidget* _parent;
    std::vector<Property>& _imported;
    std::vector<Property> _data;
    std::vector<QListWidgetItem*> _propListItems;
    bool _editModeNewItem = false;
};

#endif // PROPERTIES_H
