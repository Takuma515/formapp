package sysdes.formapp

import java.net.{Socket, URLDecoder}

import sysdes.formapp.server.{Handler, Server}

object StatelessServer extends Server(8001) {  //ポート番号を変更
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, Request, Response}



  override def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("POST", "/name", _, _, _) => name()
    case Request("POST", "/gender", _, _, Some(body)) => gender(body)
    case Request("POST", "/message", _, _, Some(body)) => message(body)
    case Request("POST", "/confirm", _, _, Some(body)) => confirm(body)
    case Request("POST", "/complete", _, _, _) => complete()
    case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  /* 1.スタート画面 */
  def index(): Response = {

    Ok("""<html lang="ja">
        |<head>
        |    <meta charset="UTF-8">
        |</head>
        |
        |<body>
        |    アンケート開始<br>
        |    <form action="/name" method="post">
        |        <input type="submit" value="start" />
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }


  /* 2.名前入力画面 */
  def name(): Response = {
    Ok(
      """<html lang="ja">
        |<head>
        |    <meta charset="UTF-8">
        |</head>
        |
        |<body>
        |    <form action="/gender" method="post">
        |    名前：<input type="text" name="name"><br>
        |    <input type="submit" value="next" />
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }

  /* 3.性別入力画面 */
  def gender(body: String): Response = {
    val parameters: Array[String] = body.split("[=&]")
    var name: String = "No Name"
    if(parameters.size==2) name = parameters(1)

    Ok(
      s"""<html lang="ja">
        |<head>
        |    <meta charset="UTF-8">
        |</head>
        |<body>
        |    <form action="/message" method="post">
        |    <input type="hidden" name="name" value="${URLDecoder.decode(name,"UTF-8")}">
        |    性別：
        |    <input type="radio" name="gender" value="male" checked> 男性
        |    <input type="radio" name="gender" value="female"> 女性<br>
        |    <input type="submit" value="next"/>
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }

  /* 4.メッセージ入力画面 */
  def message(body: String): Response = {
    val parameters: Array[String] = body.split("[=&]")
    val name: String = parameters(1)
    val gender: String = parameters(3)

    Ok(
      s"""<html lang="ja">
        |<head>
        |    <meta charset="UTF-8">
        |</head>
        |
        |<body>
        |    <form action="/confirm" method="post">
        |    <input type="hidden" name="name" value="${URLDecoder.decode(name,"UTF-8")}">
        |    <input type="hidden" name="gender" value="${URLDecoder.decode(gender,"UTF-8")}">
        |    メッセージ：<br>
        |    <textarea name="message" placeholder="メッセージを入力"></textarea><br>
        |    <input type="submit" value="next" />
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }

  /* 5.確認画面 */
  def confirm(body: String): Response = {
    val parameters: Array[String] = body.split("[=&]")
    val name: String = parameters(1)
    val gender: String = parameters(3)
    var message: String = "No Message"
    if(parameters.size==6) message = parameters(5)

    Ok(
      s"""<html lang="ja">
        |<head>
        |    <meta charset="UTF-8">
        |</head>
        |
        |<body>
        |    名前：${URLDecoder.decode(name,"UTF-8")}<br>
        |    性別：${URLDecoder.decode(gender,"UTF-8")}<br>
        |    メッセージ：${URLDecoder.decode(message,"UTF-8")}<br>
        |    <form action="/complete" method="post">
        |        <input type="submit" value="submit"/>
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }

  /*6.送信完了画面 */
  def complete(): Response = {
    Ok(
      s"""<html lang="ja">
         |<head>
         |    <meta charset="UTF-8">
         |</head>
         |
         |<body>
         |    送信が完了しました<br>
         |    <a href="/">スタート画面へ戻る</a>
         |</body>
         |</html>""".stripMargin)
  }
}
