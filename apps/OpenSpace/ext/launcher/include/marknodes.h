#ifndef MARKNODES_H
#define MARKNODES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class markNodes;
}
QT_END_NAMESPACE

class markNodes : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void parseSelections();

public:
    explicit markNodes(openspace::Profile* imported, QWidget *parent = nullptr);
    ~markNodes();

private:
    Ui::markNodes *ui;
    QWidget* _parent;
    std::vector<QListWidgetItem*> _markedNodesListItems;
    openspace::Profile* _imported;
    std::vector<std::string> _data;
};

#endif // MARKNODES_H
