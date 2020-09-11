#ifndef OSTIME_H
#define OSTIME_H

#include <QDialog>
#include <QWidget>

QT_BEGIN_NAMESPACE
namespace Ui {
class time;
}
QT_END_NAMESPACE

struct OSTime {
    enum class Type {
        Absolute,
        Relative
    };

    Type type;
    std::string time;
};

class ostime : public QDialog
{
    Q_OBJECT

public slots:
    void enableAccordingToType(int);
    void cancel();
    void approved();

public:
    explicit ostime(OSTime& imported, QWidget *parent = nullptr);
    ~ostime();
private:
    void enableFormatForAbsolute(bool enableAbs);
    Ui::time *ui;
    QWidget* _parent;
    OSTime& _imported;
    OSTime _data;
    bool _initializedAsAbsolute = true;
};

#endif // OSTIME_H
