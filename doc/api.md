# erlang-upyun

upyun erlang 版 SDK

# 安装
```sh

```

# demo
```erlang
1> c(upyun).
{ok,upyun}
2> upyun:upyun_init({"mytestbucket","testrkdtest","testrkdtest", v0}).
{ok,[{"EndPoint","v0.api.upyun.com"},
     {"PassWord","testrkdtest"},
     {"Operator","testrkdtest"},
     {"Bucket","mytestbucket"}]}
3> upyun:getUsage().
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 07:53:34 GMT"},
  {"server","vivi/0.3"},
  {"content-length","5"},
  {"content-type","text/html"},
  {"x-request-id","c9c957f1e75977ab9bcda30ee34f2ef9"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-221, ctn-zj-ngb-102"}],
 "61131"}
4> upyun:listDir().
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 07:53:47 GMT"},
  {"server","vivi/0.3"},
  {"content-length","153"},
  {"content-type","text/html"},
  {"x-request-id","23576621d9f138fe9adf7d5ea06b811a"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-226, ctn-zj-ngb-102"}],
 [["test","F","0","1421659335"],
  ["mytestbucket","F","0","1420365239"],
  ["5d175890f603738db530cb9fb01bb051f819ec28.jpg","N","7099",
   "1420011603"],
  ["119770078104f3d522l.jpg","N","39732","1420011597"]]}
5> 

```

# 文档
## API
* [`upyun_init`](#upyun_init)
* [`getUsage`](#getUsage)
* [`listDir`](#listDir)
* [`createDir`](#createDir)
* [`removeDir`](#removeDir)
* [`uploadFile`](#uploadFile)
* [`existsFile`](#existsFile)
* [`downloadFile`](#downloadFile)
* [`removeFile`](#removeFile)



# API

<a name="upyun_init" />
### upyun_init({Buckte, Operator, PassWord, EndPoint})
初始化配置
```
2> upyun:upyun_init({"mytestbucket","testrkdtest","testrkdtest", v0}).
{ok,[{"EndPoint","v0.api.upyun.com"},
     {"PassWord","testrkdtest"},
     {"Operator","testrkdtest"},
     {"Bucket","mytestbucket"}]}

```

__参数__

* `bucket`: 你要使用的 upyun 空间名字.
* `operator`: 拥有 `bucket` 授权的操作员
* `password`: 拥有 `bucket` 授权的操作员的密码
* `endpoint` API 接入点，可以刷是如下值:
  * `ctcc` 或 `v1`: 中国电信
  * `cucc` 或 `v2`: 中国联通
  * `cmcc` 或 `v3` 中国移动
  * `v0` 或 `auto` 或 任何其他的值: 将使用 `v0.api.upyun.com` （自动选择合适的线路）

---------------------------------------


<a name="getUsage" />
### getUsage()
获取空间使用状况.(单位:`Byte`)

__响应__

```erlang
3> upyun:getUsage().
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 07:53:34 GMT"},
  {"server","vivi/0.3"},
  {"content-length","5"},
  {"content-type","text/html"},
  {"x-request-id","c9c957f1e75977ab9bcda30ee34f2ef9"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-221, ctn-zj-ngb-102"}],
 "61131"}
```

注： 返回的信息包括三部分

` {Res_status, Res_head, Res_body} `

---------------------------------------

<a name="" />
### listDir(remotePath, [limit], [order], [iter], callback)
遍历指定目录. 响应结果中会包含(file or dir), size(unit: `Byte`), last modify time.

__参数__
* `remote_dir_path` 欲遍历的目录
* `limit` 限定每次请求的列表最大数目
* `order` 以 `last_modified` 的值正序或者倒序排列 `asc`(正序) 或 `desc`(倒序).(Default: `asc`)
* `iter` 遍历的起点（当指定 `limit` 小于实际文件数时，在第二次请求时候，指定此参数，即可继续上次的遍历）

__响应__

```erlang
4> upyun:listDir().
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 07:53:47 GMT"},
  {"server","vivi/0.3"},
  {"content-length","153"},
  {"content-type","text/html"},
  {"x-request-id","23576621d9f138fe9adf7d5ea06b811a"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-226, ctn-zj-ngb-102"}],
 [["test","F","0","1421659335"],
  ["mytestbucket","F","0","1420365239"],
  ["5d175890f603738db530cb9fb01bb051f819ec28.jpg","N","7099",
   "1420011603"],
  ["119770078104f3d522l.jpg","N","39732","1420011597"]]}
```

---------------------------------------

<a name="createDir" />
### createDir(remotePath)
创建文件夹
```
5> upyun:createDir("/mytestbucket/hello/").
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:17:53 GMT"},
  {"server","vivi/0.3"},
  {"content-length","0"},
  {"content-type","text/html"},
  {"x-request-id","696f9c4747ac44e41e6bba4dcba94e4d"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-082, ctn-zj-ngb-102"}],
 []}

```

---------------------------------------

<a name="removeDir" />
### removeDir(remotePath)
删除文件夹

```
6> upyun:removeDir("/mytestbucket/hello/").
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:19:02 GMT"},
  {"server","vivi/0.3"},
  {"content-length","0"},
  {"content-type","text/html"},
  {"x-request-id","1bc7fad1e056265ec4d13936e9794dec"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-222, ctn-zj-ngb-102"}],
 []}
```

---------------------------------------

<a name="uploadFile" />
### uploadFile(remotePath, localFile, type, checksum, [opts])
上传文件

__参数__
* `remotePath` 文件存放路径
* `localFile` 欲上传的文件，可以是文件的本地路径或者文件本身的内容
* `type` 指定文件的 `Content-Type`
* `checksum` 为 `true` 时 SDK 会计算文件的 md5 值并将其传于 API 校验，此外，你也可以直接指定一个 md5 值字符串
* `opts` 其他请求头部参数（以 JS 对象格式传入，常用于图片处理等需求）. 更多请参考 [官方 API 文档](http://docs.upyun.com/api/rest_api/#_4)

__响应__

```erlang
8> upyun:uploadFile("/mytestbucket/test/upyun.erl", "./upyun.erl").
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:21:01 GMT"},
  {"server","vivi/0.3"},
  {"content-length","0"},
  {"content-type","text/html"},
  {"x-request-id","22a1ffb2377af19b235f0b23a0f46f81"},
  {"x-request-path","api-php-082, ctn-zj-ngb-102"},
  {"access-control-allow-origin","*"},
  {"x-vivi-streaming","on"}],
 []}

```

---------------------------------------

<a name="downloadFile" />
### downloadFile(remotePath, [localPath], callback)
下载文件

__参数__
* `remotePath` 文件在 upyun 空间的路径
* `localPath` 文件在本地存放路径， 如果省略 `localPath` 参数，文件的内容将会直接在响应的主体中返回

```
9> upyun:downloadFile("/mytestbucket/test/upyun.erl").
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:22:17 GMT"},
  {"server","vivi/0.3"},
  {"content-length","14968"},
  {"content-type","application/octet-stream"},
  {"x-request-id","13424cbe1c9a7bfbf7bc7e2663de1602"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-226, ctn-zj-ngb-102"}],
 [37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
  37,37,37,37,37,37,37|...]}
  
  10> upyun:downloadFile("/mytestbucket/test/upyun.erl", "./a.erl").
{ok,done}

11> upyun:downloadFile("/mytestbucket/test/upyun.erl", ".hi/a.erl").
{error,enoent}

```


---------------------------------------

<a name="removeFile" />
### removeFile(remotePath, callback)
删除文件

__参数__
* `remotePath` 文件在 upyun 空间的路径

```
12> upyun:removeFile("/mytestbucket/test/upyun.erl").
{{"HTTP/1.1",200,"OK"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:24:07 GMT"},
  {"server","vivi/0.3"},
  {"content-length","0"},
  {"content-type","text/html"},
  {"x-request-id","19aea4d3a17b5be6e1834cc7e7cb3721"},
  {"access-control-allow-origin","*"},
  {"x-request-path","api-php-226, ctn-zj-ngb-102"}],
 []}
13> upyun:removeFile("/mytestbucket/test/upyun.erl").
{{"HTTP/1.1",404,"Not found"},
 [{"connection","keep-alive"},
  {"date","Tue, 20 Jan 2015 08:24:15 GMT"},
  {"server","vivi/0.3"},
  {"content-length","0"},
  {"content-type","text/html"},
  {"x-request-id","4b9fbe087a210995f76f2119fdd7fce9"},
  {"access-control-allow-origin","*"},
  {"x-error-code","-404"},
  {"x-request-path","api-php-222, ctn-zj-ngb-102"}],
 []}

```



# 备注

目前为不稳定版本，还在开发中
