#include "mainwindow.h"
#include <QDebug>

static inline void scrollDown(QTextEdit* widget)
{
    QScrollBar* sb = widget->verticalScrollBar();
    sb->setValue(sb->maximum());
}

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    m_nick("visitante"),
    m_host("localhost"),
    m_port(7000),
    m_connected(false)
{
    m_online.clear();

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

    online = new QTextEdit(this);
    online->setReadOnly(true);
    online->setGeometry(QRect(5 + (800-5-150) + 5, 5, 140, 600-5-30));

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

void MainWindow::updateOnlineUsers()
{
    online->clear();
    m_online.sort();
    for (int i=0; i < m_online.size(); ++i)
    {
        online->append(m_online.at(i));
    }
}

void MainWindow::displayMessage(QString nick, QString said)
{;
    chat->append(QString("<b>%1</b>: <font color='#222'>%2</font>").arg(nick).arg(said));
    scrollDown(chat);
}

void MainWindow::displayPvtMessage(QString nick, QString said)
{
    chat->append(QString("<b>%1[<font color='#B22'>PVT</font>]</b>: <font color='#222'>%2</font>").arg(nick).arg(said));
    scrollDown(chat);
}

void MainWindow::displaySentPvtMessage(QString nick, QString said)
{
    chat->append(QString("<b>%1[<font color='#E22'>@</font>%2]</b>: <font color='#222'>%3</font>").arg(m_nick).arg(nick).arg(said));
    scrollDown(chat);
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
        if (m_connected)
        {
            displayInfo(QString("You can't change your nick while connected!"));
            return;
        }

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
        socket->write("CONNECT:");
        socket->write(m_nick.toLatin1());
        socket->write("\n");
        displayInfo(QString("Connecting to %1 at port %2...").arg(m_host).arg(m_port));
        return;
    }
    if (cmd == "/quit")
    {
        if (!m_connected)
        {
            displayInfo(QString("You cannot disconnected. You must be connected first!"));
        }
        socket->write("QUIT:\n");
        return;
    }

    if (!m_connected)
    {
        displayInfo(QLatin1String("You're not connected. You can't send messages!"));
        return;
    }

    if (cmd.at(0) == '@')
    {
        QString who = cmd.remove(0,1);
        list.removeFirst();
        msgToSend = list.join(":");
        //qDebug() << "send msg to " << who << " = " << msgToSend;
        socket->write("PVT:");
        socket->write(who.toLatin1());
        socket->write(":");
        socket->write(msgToSend.toLatin1());
        socket->write("\n");
        displaySentPvtMessage(who, msgToSend);
        return;
    }

    socket->write("SAY:");
    socket->write(msgToSend.toLatin1());
    socket->write("\n");
    displayMessage(m_nick, msgToSend);
}

void MainWindow::readSocket()
{
    //while (true) {
        if (!socket->canReadLine())
            return;
        QString temp = socket->readLine();


        temp = temp.remove('\n');
        QStringList list = temp.split(':');
        QString cmd = list.first();

        if (cmd == "SAID" || cmd == "PVT")
        {
            QString who = list.at(1);
            list.removeFirst();
            list.removeFirst();
            QString msgRecv = list.join(":");
            if (cmd == "SAID")
                displayMessage(who, msgRecv);
            else
                displayPvtMessage(who, msgRecv);
        }
        else if (cmd == "JOIN")
        {
            QString who = list.at(1);
            displayUsage(QString("<b>%1</b> joined this channel.").arg(who));
            m_online.append(who);
            updateOnlineUsers();
        }
        else if (cmd == "LEFT")
        {
            QString who = list.at(1);
            displayUsage(QString("<b>%1</b> left this channel.").arg(who));
            m_online.removeAll(who);
            updateOnlineUsers();
        }
        else if (cmd == "CONNECT")
        {
            QString result = list.at(1);
            if (result == "OK")
            {
                m_online.clear();
                for (int i=2; i < list.size(); ++i)
                    m_online.append(list.at(i));
                updateOnlineUsers();
            }
            else
            {
                list.removeFirst();
                list.removeFirst();
                displayInfo(QString("Error trying to connect: '%1'.").arg(list.join(":")));
            }
        }

        qDebug() << "recebi: " << temp;
   // }
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
