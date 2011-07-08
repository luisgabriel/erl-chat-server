#include "mainwindow.h"

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
    setWindowTitle("Erlang Chat Room");

    socket = new QTcpSocket(this);
    connect(socket, SIGNAL(readyRead()), this, SLOT(readSocket()));
    connect(socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(errorSocket(QAbstractSocket::SocketError)));
    connect(socket, SIGNAL(connected()), this, SLOT(connected()));
    connect(socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

    chat = new QTextEdit(this);
    chat->setReadOnly(true);
    chat->setGeometry(QRect(5, 5, 645, 565));

    online = new QTextEdit(this);
    online->setReadOnly(true);
    online->setGeometry(QRect(655, 5, 140, 565));

    msg = new ChatLineEdit(this);
    msg->setGeometry(QRect(5, 575, 790, 20));

    connect(msg, SIGNAL(sendMessage(QString)), this, SLOT(sendMessage(QString)));

    displayInfo(QString("Welcome to <b>Erlang Chat Room</b> (this client is implemented in C++)."));
    displayUsage(QString("Your current nick is %1. Change it by typing <strong>/nick [new nick]</strong>.").arg(m_nick));
    displayUsage(QString("Connect to Erlang Chatroom by typing <strong>/connect [ip] [port]</strong>."));
}

void MainWindow::updateOnlineUsers()
{
    online->clear();
    QSet<QString>::const_iterator it = m_online.constEnd();

    if (it == m_online.constBegin())
        return;

    while (--it != m_online.constBegin())
        online->append(*it);

    online->append(*it);
}

void MainWindow::displayMessage(const QString& nick, const QString& said)
{
    chat->append(QString("<b>%1</b>: <font color='#222'>%2</font>").arg(nick).arg(said));
    scrollDown(chat);
}

void MainWindow::displayPvtMessage(const QString& nick, const QString& said)
{
    chat->append(QString("<b>%1[<font color='#B22'>PVT</font>]</b>: <font color='#222'>%2</font>").arg(nick).arg(said));
    scrollDown(chat);
}

void MainWindow::displaySentPvtMessage(const QString& nick, const QString& said)
{
    chat->append(QString("<b>%1[<font color='#E22'>@</font>%2]</b>: <font color='#222'>%3</font>").arg(m_nick).arg(nick).arg(said));
    scrollDown(chat);
}

void MainWindow::displayInfo(const QString& info)
{
    chat->append(QString("<font color='#d22'><b>!</b> %1</font>").arg(info));
    scrollDown(chat);
}

void MainWindow::displayUsage(const QString& usage)
{
    chat->append(QString("<font color='#22d'><b>$</b> %1</font>").arg(usage));
    scrollDown(chat);
}

void MainWindow::sendMessage(QString msgToSend)
{
    QStringList list = msgToSend.split(QString(" "));
    QString cmd = list.first().toLower();
    if (cmd[0] == '/')
    {
        if (cmd == "/nick")
        {
            if (m_connected)
                return displayInfo(QString("You can't change your nick while connected!"));

            if (list.size() > 1)
            {
                m_nick = list.at(1);
                displayInfo(QString("Your nickname was changed to <strong>%1</strong>.").arg(m_nick));
            }
            else
                displayUsage(QString("To change your nick, please type <strong>/nick [new nick]</strong>."));
        }
        else if (cmd == "/connect")
        {
            if (m_connected) {
                m_connected = false;
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
        }
        else if (cmd == "/quit")
        {
            if (!m_connected)
                return displayInfo(QString("You cannot disconnect. You must be connected first!"));

            socket->write("QUIT:\n");
        }
        else
            displayInfo(QString("The command <b>%1</b> doesn't exist. It it a typo?").arg(cmd.remove(0,1).toUpper()));
        return;
    }

    if (!m_connected)
    {
        displayInfo(QLatin1String("You can't send messages if you're not connected!"));
        return;
    }

    if (cmd.at(0) == '@')
    {
        QString who = list.first().remove(0,1);
        list.removeFirst();
        msgToSend = list.join(" ");
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
        m_online.insert(who);
        updateOnlineUsers();
    }
    else if (cmd == "LEFT")
    {
        QString who = list.at(1);
        displayUsage(QString("<b>%1</b> left this channel.").arg(who));
        m_online.remove(who);
        updateOnlineUsers();
    }
    else if (cmd == "CONNECT")
    {
        QString result = list.at(1);
        if (result == "OK")
        {
            if (!m_connected)
            {
                m_connected = true;
                displayInfo(QLatin1String("Connected! To disconnect, type <strong>/quit</strong>."));
            }

            m_online.clear();
            for (int i=2; i < list.size(); ++i)
                m_online.insert(list.at(i));
            updateOnlineUsers();
        }
        else
        {
            list.removeFirst();
            list.removeFirst();
            displayInfo(QString("Error trying to connect: %1").arg(list.join(":")));
        }
    }
}

void MainWindow::errorSocket(QAbstractSocket::SocketError)
{
    disconnected();
}

void MainWindow::connected()
{
    socket->write("CONNECT:");
    socket->write(m_nick.toLatin1());
    socket->write("\n");
}

void MainWindow::disconnected()
{
    if (m_connected)
    {
        displayInfo(QLatin1String("Disconnected!"));
        m_connected = false;
        m_online.clear();
        updateOnlineUsers();
    }
}

void MainWindow::closeEvent(QCloseEvent* event)
{
    if (m_connected)
    {
        socket->write("QUIT:\n");
        socket->close();
    }
    QMainWindow::closeEvent(event);
}
