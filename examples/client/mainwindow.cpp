#include "mainwindow.h"
#include <QDebug>
#include <QThread>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    setFixedSize(QSize(800,600));

    socket = new QTcpSocket(this);
    connect(socket, SIGNAL(readyRead()), this, SLOT(readSocket()));
    connect(socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(errorSocket(QAbstractSocket::SocketError)));
    //socket->connectToHost(QString("172.22.57.190"), 7000);
    socket->connectToHost(QString("127.0.0.1"), 7000);

    chat = new QTextEdit(this);
    msg = new QLineEdit(this);
    chat->setText("chat");
    msg->setText("oi");

    //QPushButton* button = new QPushButton("Teste",this);
    //button->show();
    //connect(button, SIGNAL(clicked()), this, SLOT(teste()));

}

MainWindow::~MainWindow()
{
}

void MainWindow::teste()
{
    socket->write("hello? world??");
}

void MainWindow::readSocket()
{
    while (true) {
        QString temp = socket->readAll();
        if (temp.isEmpty()) break;
        qDebug() << "recebi: " << temp;
    }
}

void MainWindow::errorSocket(QAbstractSocket::SocketError error)
{
    qDebug() << "erro: " << error;
}
