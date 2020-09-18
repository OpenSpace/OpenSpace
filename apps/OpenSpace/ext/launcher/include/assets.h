#ifndef ASSETS_H 
#define ASSETS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include "assettreemodel.h"
#include "filesystemaccess.h"
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class assets;
}
QT_END_NAMESPACE

class assets : public QDialog
{
    Q_OBJECT

public slots:
    void cancel();
    void parseSelections();
    void selected(const QModelIndex&);
    void setVarName();

public:
    explicit assets(openspace::Profile* imported, const std::string reportAssets,
        QWidget *parent = nullptr);
    ~assets();
    std::string createTextSummary();
    void keyPressEvent(QKeyEvent *evt);

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    void compareFilesystemWithProfileAssets();
    bool traverseToExpandSelectedItems(int nRows, QModelIndex parent);
    void findPathMatch(std::string& path, std::string& varName);
    void traverseToFindFilesystemMatch(QModelIndex parent, int nRows,
        std::string dirname, std::string varName);
    Ui::assets *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    assetTreeModel _assetTreeModel;
    QModelIndex _selectedIdx;
};

#endif // ASSETS_H
