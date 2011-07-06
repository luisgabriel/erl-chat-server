#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTcpSocket>
#include <QtGui>
#include "chatlineedit.h"

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private:
    void displayMessage(QString nick, QString said);
    void displayInfo(QString);
    void displayUsage(QString);
    QTcpSocket* socket;
    QTextEdit* chat;
    ChatLineEdit* msg;
    QString m_nick;
    QString m_host;
    int m_port;
    bool m_connected;

protected slots:
    void readSocket();
    void errorSocket(QAbstractSocket::SocketError);
    void sendMessage(QString);
    void connected();
    void disconnected();
};

#endif // MAINWINDOW_H
