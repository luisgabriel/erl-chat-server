#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTcpSocket>
#include <QTextEdit>
#include <QScrollBar>
#include <QStringList>
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
    void displayPvtMessage(QString nick, QString said);
    void displaySentPvtMessage(QString nick, QString said);
    void displayInfo(QString);
    void displayUsage(QString);
    void updateOnlineUsers();
    void closeEvent(QCloseEvent*);
    QTcpSocket* socket;
    QTextEdit* chat;
    QTextEdit* online;
    ChatLineEdit* msg;
    QString m_nick;
    QString m_host;
    int m_port;
    bool m_connected;
    QStringList m_online;

protected slots:
    void readSocket();
    void errorSocket(QAbstractSocket::SocketError);
    void sendMessage(QString);
    void connected();
    void disconnected();
};

#endif // MAINWINDOW_H
