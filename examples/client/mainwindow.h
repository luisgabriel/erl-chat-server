#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QScrollBar>
#include <QSet>
#include <QStringList>
#include <QTcpSocket>
#include <QTextEdit>

#include "chatlineedit.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);

private:
    void displayMessage(const QString& nick, const QString& said);
    void displayPvtMessage(const QString& nick, const QString& said);
    void displaySentPvtMessage(const QString& nick, const QString& said);
    void displayInfo(const QString&);
    void displayUsage(const QString&);
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
    QSet<QString> m_online;

protected slots:
    void readSocket();
    void errorSocket(QAbstractSocket::SocketError);
    void sendMessage(QString);
    void connected();
    void disconnected();
};

#endif // MAINWINDOW_H
