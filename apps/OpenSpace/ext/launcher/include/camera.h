#ifndef CAMERA_H
#define CAMERA_H

#include <QDialog>
#include <QWidget>
#include <QLineEdit>
#include <QLabel>
#include <optional>
#include <variant>

QT_BEGIN_NAMESPACE
namespace Ui {
class camera;
}
QT_END_NAMESPACE

class camera: public QDialog
{
    Q_OBJECT

public slots:
    void cancel();
    void approved();
    void tabSelect(int);

public:
    explicit camera(openspace::Profile* imported, QWidget *parent = nullptr);
    ~camera();
    enum class cameraTypeTab : int {
        Nav = 0,
        Geo
    };
    void keyPressEvent(QKeyEvent *evt);

private:
    bool isNumericalValue(QLineEdit* le);
    bool inNumericalRange(QLineEdit* le, float min, float max);
    bool areRequiredFormsFilledAndValid();
    void setErrorTextFormat(QLabel* label, const QString& labelTxt, bool setErrorFormat);
    void checkFormFilled(QLabel* label, QLineEdit* value, const QString& labelTxt,
        bool& allFormsValid, bool isNumber, bool isRequiredValue);
    void checkFormRange(QLabel* label, QLineEdit* value, const QString& labelTxt,
        float min, float max, bool& allFormsValid, bool isRequiredValue);
    bool isUpVectorValid();

    Ui::camera *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    openspace::Profile::CameraType _data;
};

#endif // CAMERA_H
