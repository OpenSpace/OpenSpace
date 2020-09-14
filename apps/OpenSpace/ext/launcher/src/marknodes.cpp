#include "marknodes.h"
#include "./ui_marknodes.h"
#include <qevent.h>
#include <iterator>

markNodes::markNodes(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::markNodes)
    , _imported(imported)
    , _data(imported->markNodes())
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        _markedNodesListItems.push_back(
            new QListWidgetItem(QString(_data[i].c_str()))
        );
        ui->list->addItem(_markedNodesListItems[i]);
    }

    ui->line_node->setText("");

    connect(ui->list, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(listItemAdded()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(listItemRemove()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
}

void markNodes::listItemSelected(void) {
    ui->button_remove->setDisabled(false);
}

void markNodes::listItemAdded(void) {
    if (ui->line_node->text().length() == 0) {
        return;
    }

    std::string itemToAdd = ui->line_node->text().toUtf8().constData();
    auto idx = std::find(_data.begin(), _data.end(), itemToAdd);
    if (idx != _data.end()) {
        ui->list->setCurrentRow(std::distance(_data.begin(), idx));
    }
    else {
        _data.push_back(itemToAdd);
        _markedNodesListItems.push_back(new QListWidgetItem(ui->line_node->text()));
        ui->list->addItem(_markedNodesListItems.back());

        //Scroll down to that blank line highlighted
        ui->list->setCurrentItem(_markedNodesListItems.back());
    }

    //Blank-out entry again
    ui->line_node->setText("");
}

void markNodes::listItemRemove(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if (index < 0 || index >= _markedNodesListItems.size()) {
        return;
    }

    ui->list->takeItem(index);
    _data.erase(_data.begin() + index);
    _markedNodesListItems.erase(_markedNodesListItems.begin() + index);
}

void markNodes::parseSelections() {
    _imported->setMarkNodes(_data);
    accept();
}

markNodes::~markNodes() {
    for (auto n : _markedNodesListItems) {
        delete n;
    }
    delete ui;
}
