#ifndef CAMERA_H
#define CAMERA_H

#include <QDialog>
#include <QWidget>
#include <QLineEdit>
#include <QLabel>
#include <optional>

QT_BEGIN_NAMESPACE
namespace Ui {
class camera;
}
QT_END_NAMESPACE

struct CameraNavState {
    static constexpr const char* Type = "setNavigationState";

    std::string anchor;
    std::string aim;
    std::string referenceFrame;
    std::string position[3];
    std::string up[3];
    std::string yaw;
    std::string pitch;
};

struct CameraGoToGeo {
    static constexpr const char* Type = "goToGeo";

    std::string anchor;
    std::string latitude;
    std::string longitude;
    std::string altitude;
};

struct Camera {
    enum class Type {
        Nav,
        Geo
    };
    Type type;
    CameraNavState nav;
    CameraGoToGeo geo;
};

class camera: public QDialog
{
    Q_OBJECT

public slots:
    void cancel();
    void approved();

public:
    explicit camera(Camera& imported, QWidget *parent = nullptr);
    ~camera();

private:
    bool isNumericalValue(QLineEdit* le);
    bool inNumericalRange(QLineEdit* le, float min, float max);
    bool areRequiredFormsFilledAndValid();
    void setErrorTextFormat(QLabel* label, const QString& labelTxt, bool setErrorFormat);
    void checkFormFilled(QLabel* label, QLineEdit* value, const QString& labelTxt,
        bool& allFormsValid, bool isNumber, bool isRequiredValue);
    void checkFormRange(QLabel* label, QLineEdit* value, const QString& labelTxt,
        float min, float max, bool& allFormsValid, bool isRequiredValue);

    Ui::camera *ui;
    QWidget* _parent;
    Camera& _imported;
    Camera _data;
};

#endif // CAMERA_H
