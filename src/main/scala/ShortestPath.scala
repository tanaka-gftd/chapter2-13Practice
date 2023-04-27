//2-13
//初級問題 画像で与えられたグラフの最短経路を、ベルマンフォード法で解く
//中級問題 画像で与えられたグラフの最短経路を、ダイクストラ法で解く  //中級問題 惜しい
//上級問題 画像で与えられたグラフの最短経路を、ワーシャルフロイド法で解く  //上級問題 難しかった

case class Edge(from: Char, to: Char, distance: Int)

object ShortestPath {
  val vertexes = 'A' to 'N'
  val edges = Seq(
    Edge('A', 'B', 9),
    Edge('A', 'C', 6),
    Edge('A', 'D', 6),
    Edge('B', 'A', 9),
    Edge('B', 'E', 2),
    Edge('C', 'A', 6),
    Edge('C', 'E', 9),
    Edge('C', 'G', 6),
    Edge('D', 'A', 6),
    Edge('D', 'F', 3),
    Edge('E', 'B', 2),
    Edge('E', 'C', 9),
    Edge('E', 'I', 1),
    Edge('F', 'D', 3),
    Edge('F', 'H', 5),
    Edge('F', 'J', 9),
    Edge('G', 'C', 6),
    Edge('G', 'I', 3),
    Edge('G', 'J', 9),
    Edge('H', 'F', 5),
    Edge('H', 'K', 5),
    Edge('I', 'E', 1),
    Edge('I', 'G', 3),
    Edge('J', 'F', 9),
    Edge('J', 'G', 9),
    Edge('J', 'K', 4),
    Edge('J', 'L', 7),
    Edge('J', 'M', 6),
    Edge('K', 'H', 5),
    Edge('K', 'J', 4),
    Edge('K', 'M', 1),
    Edge('L', 'J', 7),
    Edge('L', 'N', 3),
    Edge('M', 'J', 6),
    Edge('M', 'K', 1),
    Edge('M', 'N', 2),
    Edge('N', 'L', 3),
    Edge('N', 'M', 2)
  )


  //初級問題 与えられたグラフの単一始点最短経路問題を、ベルマンフォード法で解く
  def solveByBellmanFord(start: Char, goal: Char): Unit = {

    //各ノードへの最短距離を保存するMapを作成(始点ノードのみ0で、それ以外のノードはInt.MaxValueで擬似的に♾️とする)
    var distances = vertexes.map(_ -> Int.MaxValue).toMap
    distances = distances + (start -> 0)

    var isUpdated = true

    while (isUpdated) {
      isUpdated = false
      edges.foreach { (e) =>
        if ((distances(e.from) != Int.MaxValue) && (distances(e.to) > distances(e.from) + e.distance)) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))
          isUpdated = true
        }
      }
    }
    println(distances)
    println(distances(goal))
  }



  //中級問題 画像で与えられたグラフの最短経路を、ダイクストラ法で解く
  /*
    一度最短距離を更新してしまうと、さらに短い距離で更新できなくなるので、その時点で最短距離の更新処理が止まってしまう

    原因判明：
    最短距離更新時、e.to側ノードの更新可能フラグをオフにしているから。
    結果、e.to側のノードが、e.fromに格納された際、更新可能フラグがオフのため、最短距離が更新されなくなる。

    解決策：
    最短距離更新時、e.from側の更新可能フラグをオフにする必要がある
  */

  def solveByDijkstra(start: Char, goal: Char): Unit = {

    /*
     「keyがノード, valueが最短距離と最短距離が更新可能かどうかのフラグ」となるようなMapを作成したいので、
      value部分を保持するためのケースクラスを宣言しておく
    */
    case class costWithFlag(cost: Int, canUpdated: Boolean)  //canUpdatedがtrueで、最短距離更新可能ノードとする

    //「ノードがkey, 最短距離と最短距離が更新可能かどうかのフラグをvalue」となるようなMapを作成
    //最短距離更新可能フラグの初期値はtrueで、更新可能としておく(始点のみ、最短距離は0)
    var distances = vertexes.map(_ -> costWithFlag(Int.MaxValue, canUpdated = true)).toMap
    distances = distances + (start -> costWithFlag(0, canUpdated = true))

    var isUpdated = true

    while (isUpdated) {
      isUpdated = false

      /*
        最短距離が更新するかどうかの条件に、
        ベルマンフォードで使用した
        •「辺の始点までの最短距離」が無限大ではない場合
        •「辺の終点までの最短距離」が「辺の始点までの最短距離」+「辺の距離」より大きい
        の2条件に、以下の条件も追加する
        •更新可能フラグがtrueになっている事
      */
      edges.foreach { (e) =>
        if (
          distances(e.to).canUpdated &&  //更新可能フラグがtrueになっている
          distances(e.from).cost != Int.MaxValue &&
          distances(e.to).cost > distances(e.from).cost + e.distance
         )
        {
          //移動先ノード(e.to)への最短距離更新
          distances = distances + (e.to -> costWithFlag(distances(e.from).cost + e.distance, canUpdated = true))

          //移動元ノード(e.from)の更新可能フラグをオフにする
          distances = distances + (e.from -> costWithFlag(distances(e.from).cost, canUpdated = false))

          isUpdated = true
        }
      }
    }

    //println表示のため、map関数でHashMapを改良
    //ここで使用しているcaseは、パターンマッチのcase(match句まわりは省略できる)
    println(distances.map {
      case (key, value) => (key -> value.cost)
    })
    //match句まわりを省略しない書き方
    /*
    println(distances.map {e => e match {
      case (key, value) => (key -> value.cost)}
    })
    */

    println(distances(goal).cost)
  }



  //中級問題 模範解答
  /*
    更新済みノードを保管するためのコレクション(Set)を作り、
    最短距離を更新する際には、そのノードがSetに含まれていないかどうかチェックする
    Set...値の集合を提供するデータ構造で、重複する値を容認しない
  */
  /*
    基本的には私が作ったものと同じなのに、こちらは最後まで処理が続く。何故？
    ↓
    原因判明
    私が作ったコードでは、移動先(e.to側)に更新不可フラグを付与していたから。、
    更新不可フラグの付与先を移動元(e.from側)に変更したら、うまく動きました。
  */
  def solveByDijkstra_2(start: Char, goal: Char): Unit = {

    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    distances = distances + (start -> 0)

    //更新済みのノードの情報を保管しておくSet
    //更新済みとなるのは移動元ノード側(e.from側)
    var usedEdges: Set[Edge] = Set()

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>

        //最短距離更新可能ノードかどうかの判定に、usedEdges.contains(e) を使用している
        //usedEdges.contains(e)がfalse、つまりusedEdgesに引数eの情報含まれていなければ、最短距離更新可能ノードとする
        if (!usedEdges.contains(e)
          && distances(e.from) != Int.MaxValue
          && distances(e.to) > distances(e.from) + e.distance) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))

          //更新済み保存用Setに、更新済みノードの情報を保存
          usedEdges = usedEdges + e

          isUpdated = true
        }
      }
    }
    println(distances)
    println(distances(goal))
  }



  //上級問題 画像で与えられたグラフの最短経路を、ワーシャルフロイド法で解く
  def solveByWarshallFloyd(start: Char, goal: Char): Unit = {

    /*
      まず、自ノードへの移動コストを0として、自ノードへの移動コストまとめたMapを作る。
      次に、2つのノード間の距離をまとめたMapを作る(=存在する全ての辺と、その辺の移動コストをまとめたMapを作る)
      作成した2つのMapを、++メソッドで結合させる
    */
    var distanceMap: Map[(Char, Char), Int] = vertexes.map(v => ((v, v) -> 0)).toMap
    distanceMap = distanceMap ++ edges.map(e => (e.from, e.to) -> e.distance)

    /*
      引数で渡した2つのノードで作られる辺が、
      distanceMapに存在すれば、その辺の移動コストを、
      distanceMapに存在しなければ、Int.MaxValue/2(無限大の代用)を返すような関数を定義する。
      (Int.MaxValueを使わないのは、Int.MaxValue同士で足し算が起きた場合、Int値がオーバーフローが起きてしまうから)
      getOrElse()...呼び出し側mapに、第1引数で渡したKeyが存在すればvalueを返し、存在しなければ第2引数で渡した値を返す
    */
    def distance(v1: Char, v2: Char): Int = distanceMap.getOrElse((v1, v2), Int.MaxValue / 2)

    /*
      3重ループ for (v1 <- vertexes; v2 <- vertexes; v3 <- vertexes){処理}

      3重ループのイメージ図
      for(v1){
        for(v2){
          for(v3){
            処理
          }
        }
      }

      vertexesはRangeオブジェクトなので、for文の条件としてそのまま使用できる。
      v1,v2,v3の各初期値は'A'で、最大値の'N'までループ。
      最初は、一番内側のv3の値がAからNへと増えていく
      (Scalaでは、アルファベットの順番でもfor文の条件として使用できるようだ)
      v1：中継点ノード
      v2：始点ノード
      v3：終点ノード
    */
    for (v1 <- vertexes; v2 <- vertexes; v3 <- vertexes) {

      /*
       「始点ノードから終点ノード」と「始点ノードから中継点ノードを経由して終点ノード」の2つの移動コストを比較して、
        移動コストが小さい方を2点の最短移動距離としてdistanceMapに記録(更新)する
      */
      distanceMap = distanceMap + ((v2, v3) -> math.min(distance(v2, v3), distance(v2, v1) + distance(v1, v3)))
    }

    //結果をコンソールに表示
    println(distanceMap)
    println(distanceMap((start, goal)))
  }
}
