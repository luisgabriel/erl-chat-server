#include "chatlineedit.h"
#include <QDebug>

ChatLineEdit::ChatLineEdit(QWidget* parent)
    : QLineEdit(parent)
{

}

ChatLineEdit::~ChatLineEdit()
{

}

void ChatLineEdit::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Return)
    {
        emit sendMessage(text());
        setText(QString());
        return;
    }

    QLineEdit::keyPressEvent(event);
}
