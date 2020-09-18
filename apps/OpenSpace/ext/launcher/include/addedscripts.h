#ifndef ADDEDSCRIPTS_H
#define ADDEDSCRIPTS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class addedScripts;
}
QT_END_NAMESPACE

class addedScripts : public QDialog
{
    Q_OBJECT

public slots:
    void parseScript();

public:
    explicit addedScripts(openspace::Profile* imported, QWidget *parent = nullptr);
    ~addedScripts();
    void setScriptText(std::string s);
    void keyPressEvent(QKeyEvent *evt);

private:
    Ui::addedScripts *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    std::string _data;
};

#endif // ADDEDSCRIPTS_H
