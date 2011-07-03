#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTcpSocket>
#include <QtGui>

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
    QTcpSocket* socket;
    QTextEdit* chat;
    QLineEdit* msg;

protected slots:
    void readSocket();
    void errorSocket(QAbstractSocket::SocketError);
    void teste();
};

#endif // MAINWINDOW_H
