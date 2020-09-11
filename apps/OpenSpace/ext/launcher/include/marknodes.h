#ifndef MARKNODES_H
#define MARKNODES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

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
    explicit markNodes(std::vector<std::string>& imported, QWidget *parent = nullptr);
    ~markNodes();

private:
    Ui::markNodes *ui;
    QWidget* _parent;
    std::vector<QListWidgetItem*> _markedNodesListItems;
    std::vector<std::string>& _imported;
    std::vector<std::string> _data;
};

#endif // MARKNODES_H
