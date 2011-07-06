#include "mainwindow.h"
#include <QDebug>
#include <QThread>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    m_nick("visitante"),
    m_host("localhost"),
    m_port(7000),
    m_connected(false)
{
    setFixedSize(QSize(800,600));

    socket = new QTcpSocket(this);
    connect(socket, SIGNAL(readyRead()), this, SLOT(readSocket()));
    connect(socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(errorSocket(QAbstractSocket::SocketError)));
    connect(socket, SIGNAL(connected()), this, SLOT(connected()));
    connect(socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

    //socket->connectToHost(QString("172.22.57.190"), 7000);
    //socket->connectToHost(QString("127.0.0.1"), 7000);

    chat = new QTextEdit(this);
    chat->setReadOnly(true);
    chat->setGeometry(QRect(5, 5, 800-5-150, 600-5-30));

    msg = new ChatLineEdit(this);
    msg->setGeometry(QRect(5, 5+chat->height()+5, 800-5-5, 20));

    connect(msg, SIGNAL(sendMessage(QString)), this, SLOT(sendMessage(QString)));

    displayInfo(QString("Welcome to the Erlang Client Chat (implemented in C++)."));
    displayUsage(QString("Your current nick is %1. Change it by typing <strong>/nick [new nick]</strong>.").arg(m_nick));
    displayUsage(QString("Connect to Erlang Chatroom by typing <strong>/connect [ip] [port]</strong>."));

}

MainWindow::~MainWindow()
{
}

void MainWindow::displayMessage(QString nick, QString said)
{;
    chat->append(QString("<b>%1</b>: <font color='#222'>%2</font>").arg(nick).arg(said));
    QScrollBar* sb = chat->verticalScrollBar();
    sb->setValue(sb->maximum());
}

void MainWindow::displayInfo(QString info)
{
    chat->append(QString("<font color='#d22'><b>!</b> %1</font>").arg(info));
    QScrollBar* sb = chat->verticalScrollBar();
    sb->setValue(sb->maximum());
}

void MainWindow::displayUsage(QString usage)
{
    chat->append(QString("<font color='#22d'><b>$</b> %1</font>").arg(usage));
    QScrollBar* sb = chat->verticalScrollBar();
    sb->setValue(sb->maximum());
}

void MainWindow::sendMessage(QString msgToSend)
{
    //qDebug() << "I should send " << msgToSend;
    QStringList list = msgToSend.split(QString(" "));
    QString cmd = list.first().toLower();
    if (cmd == "/nick")
    {
        if (list.size() > 1)
        {
            m_nick = list.at(1);
            displayInfo(QString("Now your nick is <strong>%1</strong>.").arg(m_nick));
        }
        else
        {
            displayUsage(QString("To change your nick, please type <strong>/nick [new nick]</strong>."));
        }
        return;
    }
    else if (cmd == "/connect")
    {
        if (m_connected)
        {
            socket->close();
        }

        if (list.size() > 1)
        {
            QString host = list.at(1);
            if (!host.isEmpty())
                m_host = host;

            if (list.size() > 2)
            {
                QString port = list.at(2);
                if (!port.isEmpty())
                    m_port = port.toInt();
            }
        }

        socket->connectToHost(m_host, m_port);
        displayInfo(QString("Connecting to %1 at port %2...").arg(m_host).arg(m_port));
        return;
    }

    if (!m_connected)
    {
        displayInfo(QLatin1String("You're not connected. You can't send messages!"));
    }



    socket->write(msgToSend.toLatin1());
    socket->write("\n");
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
    qDebug() << "error: " << error;
    m_connected = false;
    displayInfo("Disconnected!");
}

void MainWindow::connected()
{
    m_connected = true;
    displayInfo(QLatin1String("Connected!"));
}

void MainWindow::disconnected()
{
    m_connected = false;
    displayInfo(QLatin1String("Disconnected!"));
}
