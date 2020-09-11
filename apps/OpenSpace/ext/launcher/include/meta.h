#ifndef META_H
#define META_H

#include <QDialog>
#include <QWidget>

QT_BEGIN_NAMESPACE
namespace Ui {
class meta;
}
QT_END_NAMESPACE

struct Meta {
    std::string name;
    std::string version;
    std::string description;
    std::string author;
    std::string url;
    std::string license;
};

class meta : public QDialog
{
    Q_OBJECT

public slots:
   void save();

public:
    explicit meta(Meta& imported, QWidget *parent = nullptr);
    ~meta();

private:
    Ui::meta *ui;
    QWidget* _parent;
    Meta& _imported;
};

#endif // META_H
