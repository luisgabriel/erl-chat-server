#ifndef CHATLINEEDIT_H
#define CHATLINEEDIT_H


#include <QMainWindow>
#include <QLineEdit>
#include <QKeyEvent>

class ChatLineEdit : public QLineEdit
{
    Q_OBJECT

public:
    explicit ChatLineEdit(QWidget* parent = 0);
    ~ChatLineEdit();

signals:
    void sendMessage(QString);

private:
    void keyPressEvent(QKeyEvent*);
};


#endif // CHATLINEEDIT_H
