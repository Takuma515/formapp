# Webアプリケーションの基礎

## HTTP通信における状態管理
入力フォームを持った簡単なアプリケーションの実装を通して，以下２通りの通信の状態管理方法を学ぶ．

1. 通信に状態を載せる方式
2. セッションを管理する方式

## 要求仕様
作成するサービスは以下の5つのページからなる．

1. スタート画面：「start」ボタンを押すことで名前入力画面へ遷移する．
2. 名前入力画面：名前を入力するテキストフィールドを持ち，「next」ボタンを押すことで性別入力画面へ遷移する．
3. 性別入力画面：性別を選択するラジオボタンを持ち，「next」ボタンを押すことでメッセージ入力画面へ遷移する．
4. メッセージ入力画面：メッセージを入力するテキストエリアを持ち，「next」ボタンを押すことで確認画面へ遷移する．
5. 確認画面：これまでに入力された名前，性別，メッセージを表示し，「submit」ボタンを押すことでスタート画面に戻る．

## 発展仕様
2-5ページに「back」ボタンを設置し，前のページにおける入力をやり直せるようにせよ．
