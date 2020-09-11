#ifndef ADDEDSCRIPTS_H
#define ADDEDSCRIPTS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

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
    explicit addedScripts(std::string& imported, QWidget *parent = nullptr);
    ~addedScripts();
    void setScriptText(std::string s);

private:
    Ui::addedScripts *ui;
    QWidget* _parent;
    std::string& _imported;
};

#endif // ADDEDSCRIPTS_H
