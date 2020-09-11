#ifndef META_H
#define META_H

#include <openspace/scene/profile.h>
#include <QDialog>
#include <QWidget>

QT_BEGIN_NAMESPACE
namespace Ui {
class meta;
}
QT_END_NAMESPACE

class meta : public QDialog
{
    Q_OBJECT

public slots:
   void save();

public:
    explicit meta(openspace::Profile::Meta& imported, QWidget *parent = nullptr);
    ~meta();

private:
    Ui::meta *ui;
    QWidget* _parent;
    openspace::Profile::Meta& _imported;
};

#endif // META_H
