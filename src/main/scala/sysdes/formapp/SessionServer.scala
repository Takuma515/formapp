package sysdes.formapp

import java.net.{Socket, URLDecoder}
import java.util
import java.util.UUID

import sysdes.formapp.SessionServerHandler.states
import sysdes.formapp.server.{Handler, Server}

import scala.collection.mutable.HashMap
import scala.util.control.Breaks

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states = HashMap[UUID, State]()
}

//名前、性別、メッセージを保存するオブジェクト
class State(var name:String, var gender: String, var message: String)

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, Request, Response}
  def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("POST", "/name", _, _, _) => name(request)
    case Request("POST", "/gender", _, _, Some(body)) => gender(body,request)
    case Request("POST", "/message", _, _, Some(body)) => message(body,request)
    case Request("POST", "/confirm", _, _, Some(body)) => confirm(body,request)
    case Request("POST", "/complete", _, _, _) => complete()
    case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  //ステートをHashMapに追加
  def createState(sessionID: UUID): Unit ={
    states.addOne(sessionID,new State("No Name","male","No message"))
  }

  //ステートの取得
  def getState(sessionID: UUID): State = {
    states.apply(sessionID)
  }

  //クッキーからセッションIDを取得
  def getSessionID(request: Request) : UUID = {
    val Cookie = request.headers.get("Cookie").get.replaceAll(" ","") //空白除去
    val Cookie_parameters = Cookie.split("[;=]")
    var sessionID: UUID = null
    for (i <- 0 to Cookie_parameters.size-1){
      if (Cookie_parameters(i).equals("sessionID")) sessionID = UUID.fromString(Cookie_parameters(i+1))
    }
    return sessionID
  }

  def index(): Response = {
    val sessionID: UUID = UUID.randomUUID() //セッションIDの生成
    createState(sessionID) //ステート(状態)インスタンスの生成
    val response = Ok(
      s"""<html lang="ja">
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
    response.addHeader("Set-Cookie","sessionID="+sessionID.toString) //セッションキーをクッキーで共有
    return response
  }

  /* 2.名前入力画面 */
  def name(request: Request): Response = {
    Ok(
      s"""<html lang="ja">
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
  def gender(body: String,request: Request): Response = {
    val parameters: Array[String] = body.split("[=&]")
    val sessionID: UUID = getSessionID(request)  //クッキーからセッションIDを取得
    if(parameters.size==2) getState(sessionID).name = URLDecoder.decode(parameters(1),"UTF-8")

    Ok(
      s"""<html lang="ja">
         |<head>
         |    <meta charset="UTF-8">
         |</head>
         |<body>
         |    <form action="/message" method="post">
         |    性別：
         |    <input type="radio" name="gender" value="male" checked> 男性
         |    <input type="radio" name="gender" value="female"> 女性<br>
         |    <input type="submit" value="next"/>
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  /* 4.メッセージ入力画面 */
  def message(body: String,request: Request): Response = {
    val parameters: Array[String] = body.split("[=&]")
    val sessionID: UUID = getSessionID(request)
    if(parameters.size==2) getState(sessionID).gender = URLDecoder.decode(parameters(1),"UTF-8")

    Ok(
      s"""<html lang="ja">
         |<head>
         |    <meta charset="UTF-8">
         |</head>
         |
         |<body>
         |    <form action="/confirm" method="post">
         |    メッセージ：<br>
         |    <textarea name="message" placeholder="メッセージを入力"></textarea><br>
         |    <input type="submit" value="next" />
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  /* 5.確認画面 */
  def confirm(body: String,request: Request): Response = {
    val parameters: Array[String] = body.split("[=&]")
    val sessionID: UUID = getSessionID(request)
    if(parameters.size==2) getState(sessionID).message = URLDecoder.decode(parameters(1),"UTF-8")
    val state: State = getState(sessionID)

    Ok(
      s"""<html lang="ja">
         |<head>
         |    <meta charset="UTF-8">
         |</head>
         |
         |<body>
         |    名前：${state.name}<br>
         |    性別：${state.gender}<br>
         |    メッセージ：${state.message}<br>
         |    <form action="/complete" method="post">
         |        <input type="submit" value="submit"/>
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  /*6.送信完了画面 */
  def complete(): Response = {
    // System.out.println(states.size)  //インスタンス共有の確認
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
