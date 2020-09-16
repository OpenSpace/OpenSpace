#ifndef DELTATIMES_H
#define DELTATIMES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class deltaTimes;
}
QT_END_NAMESPACE

static const double _defaultDeltaTimes[30] = {
    1, 2, 5, 10, 30,
    60, 120, 300, 600, 1800,
    3600, 7200, 10800, 21600, 43200,
    86400, 172800, 345600, 604800, 1209600,
    2592000, 5184000, 7776000, 15552000, 31536000,
    63072000, 157680000, 315360000, 630720000, 1576800000
};

struct DeltaTimes {
    std::vector<double> _times;
    DeltaTimes() {
        _times.resize(sizeof(_defaultDeltaTimes)/sizeof(double));
	zeroValues();
    };
    DeltaTimes(std::vector<double> dt) {
        _times.resize(sizeof(_defaultDeltaTimes)/sizeof(double));
	zeroValues();
        _times = dt;
    };
    void loadValues(std::vector<double>& dt) {
        for (size_t i = 0; i < dt.size(); ++i) {
            _times[i] = dt[i];
        }
    }
    size_t size() {
        auto it = find(_times.begin(), _times.end(), 0);
        return std::distance(_times.begin(), it);
    };
    size_t totalSize() {
        return (sizeof(_defaultDeltaTimes) / sizeof(double));
    }
    void zeroValues() {
        for (size_t i = 0; i < _times.size(); ++i) {
            _times[i] = 0.0;
        }
    }
};

class deltaTimes : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void saveDeltaTimeValue();
    void clearDeltaTimeValue();
    void parseSelections();

public:
    explicit deltaTimes(openspace::Profile* _imported, QWidget *parent = nullptr);
    ~deltaTimes();
    void setDeltaTimes(std::vector<double>& dt);
    QString createSummaryForDeltaTime(size_t idx, double dt, bool forListView);
    struct timeIntervals {
        int index;
        double secondsPerInterval;
        QString intervalName;
    };

    const int secondsPerYear = 31536000;
    const int secondsPerMonth = 18144000;
    const int secondsPerWeek = 604800;
    const int secondsPerDay = 86400;
    const int secondsPerHour = 3600;
    const int secondsPerMinute = 60;

private:
    QString timeDescription(double value);
    bool checkForTimeDescription(QString& description, QString unit,
        int interval, double value);
    QString checkForTimeDescription(int intervalIndex, double value);
    int lastSelectableItem();
    bool isNumericalValue(QLineEdit* le);

    Ui::deltaTimes *ui;
    QWidget* _parent;

    openspace::Profile* _imported;
    DeltaTimes _data;
    std::vector<std::string> _deltaTimeStrings;
    std::vector<QListWidgetItem*> _deltaListItems;

    std::vector<timeIntervals> _timeIntervals = {
        {0, 31536000, "year"},
        {1, 18144000, "month"},
        {2, 604800,   "week"},
        {3, 86400,    "day"},
        {4, 3600,     "hour"},
        {5, 60,       "minute"},
        {6, 1,        "second"},
    };
};

#endif // DELTATIMES_H
