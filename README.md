# UPYUN Erlang SDK

[![Build Status](https://travis-ci.org/upyun/erlang-sdk.svg?branch=master)](https://travis-ci.org/upyun/erlang-sdk)

UPYUN Erlang SDK 集成：

  - [UPYUN HTTP REST 接口](https://docs.upyun.com/api/rest_api/)
  - [UPYUN 缓存刷新接口](https://docs.upyun.com/api/purge/)

## 安装说明

确保安装了 17.1 版本或以上的 Erlang/OTP 环境。

#### 配置 `rebar.config` 文件

```erlang
{deps, [
    ...
    {upyun, ".*", {git, "git://github.com/upyun/erlang-sdk.git", {branch, "master"}}}
]}.
```

#### 编译和测试

```bash
git clone https://github.com/upyun/erlang-sdk.git && cd erlang-sdk
./rebar3 compile
./rebar3 dialyzer
./rebar3 eunit
```

## 接口说明

所有的接口调用（特殊注明除外），成功会返回 `ok` 或 `{ok, Result}`，失败则返回 `{error, Reason}`，其中 `Reason` 包含了完整的 HTTP 返回请求信息，错误代码参看 [API 错误码表](https://docs.upyun.com/api/errno/)。

用法示例参看[测试文件](./test/upyun_tests.erl)。

#### 初始化服务

```erlang
{ok, Client} = upyun:init(Options).
```

参数说明：

  - Options，`proplists` 类型的参数：
    - bucket，必选，服务空间名称
    - operator，必选，授权操作员帐号
    - password，必选，授权操作员密码
    - endpoint，网络接入点，其可选的值有：
      - `auto`，根据网络条件自动选择接入点，默认
      - `telecom`，电信接入点
      - `cnc`，联通网通接入点
      - `ctt`，移动铁通接入点

返回值说明：

  - Client，包含完整配置信息的客户端对象，`maps` 类型

#### 上传文件

```erlang
ok = upyun:put(Client, Key, FileBin).
```

参数说明：

  - Key，文件在又拍云存储服务中保存的路径
  - FileBin，待上传文件的二进制数据
  - Options，可选，`proplists` 类型的参数：
    - checksum，可选，`boolean` 类型，默认 `false`，表示不对上传的数据进行 MD5 校验
    - secret，可选，指定具体密钥内容；默认 `false`，表示不设置密钥。特别地，该功能仅对配置了缩略图版本号的图片空间有效
    - headers，可选，根据需求设置自定义 HTTP Header，例如作图参数 `x-gmkerl-*`，具体参看[上传预处理（同步）](https://docs.upyun.com/cloud/image/#sync_upload_process)

#### 流式上传

###### 上传本地文件

```erlang
ok = upyun:put(Client, Key, {stream, {file, FileName}}, Options).
```

参数说明：

  - FileName，待上传文件的路径
  - Options，可选，`proplists` 类型的参数：
    - chunk_size，可选，指定数据流分块大小，默认 `4M`
    - secret，可选，指定具体密钥内容；默认 `false`，表示不设置密钥。特别地，该功能仅对配置了缩略图版本号的图片空间有效
    - headers，可选，根据需求设置自定义 HTTP Header，例如作图参数 `x-gmkerl-*`，具体参看[上传预处理（同步）](https://docs.upyun.com/cloud/image/#sync_upload_process)

###### 自定义上传

```erlang
ok = upyun:put(Client, Key, {stream, StreamFun}, Options).
```

参数说明：

  - StreamFun，自定义 streaming 读取函数，具体参看[相关测试用例](./test/upyun_tests.erl)
  - Options，可选，`proplists` 类型的参数：
    - secret，可选，指定具体密钥内容；默认 `false`，表示不设置密钥。特别地，该功能仅对配置了缩略图版本号的图片空间有效
    - headers，可选，根据需求设置自定义 HTTP Header，例如作图参数 `x-gmkerl-*`，具体参看[上传预处理（同步）](https://docs.upyun.com/cloud/image/#sync_upload_process)

#### 断点续传

适用于上传大文件，文件在上传过程中发生异常失败时，将等待 100 毫秒后，在失败断点处自动重试 2 次。  
除此之外，也提供了更加灵活的三阶段上传接口，便于自定义断点续传实现，具体参看[断点续传模块](./src/upyun_resume.erl)。

```erlang
ok = upyun:put(Client, Key, FileBin, Options).
```

参数说明：

  - FileBin，待上传文件的二进制数据
  - Options，`proplists` 类型的参数：
    - resume，必选，值为 `true`，标记此次上传为断点续传
    - type，必选，MIME 文件类型，如 `<<"image/jpeg">>`
    - checksum，可选，默认 `false`，表示不对上传的数据进行 MD5 校验

#### 下载文件

```erlang
{ok, FileBin} = upyun:get(Client, Key).
```
返回值说明：

  - FileBin，下载到的二进制数据

#### 流式下载

###### 下载到本地文件

```erlang
ok = upyun:get(Client, Key, [{stream, {file, FileName}}]).
```

参数说明：

  - FileName，文件内容写入本地文件的路径

###### 自定义下载

```erlang
ok = upyun:get(Client, Key, [{stream, StreamFun}]).
```

参数说明：

  - StreamFun，自定义 streaming 写入函数，具体参看[相关测试用例](./test/upyun_tests.erl)

#### 删除文件

```erlang
ok = upyun:delete(Client, Key).
```

同步删除有频率限制，批量删除推荐下面这个异步删除接口（无频率限制）。

```erlang
ok = upyun:delete(Client, Key, true).
```

#### 获取文件信息

```erlang
{ok, {Type, Size, LastModified}} = upyun:getinfo(Client, Key).
```

返回值说明：

  - Type，文件为 `file`，文件夹为 `directory`
  - Size，文件大小
  - LastModified，文件最后修改时间

#### 创建目录

```erlang
ok = upyun:mkdir(Client, DirKey).
```

参数说明：

  - DirKey，待创建的目录路径

#### 删除目录

```erlang
ok = upyun:rmdir(Client, DirKey).
```

#### 获取目录文件列表

```erlang
{ok, {Result, Iter}} = upyun:getlist(Client, DirKey, Options).
```

参数说明：

  - DirKey，目录路径
  - Options，`proplists` 类型的参数：
    - iter，分页开始位置，通过上次请求的返回值中获得，所以第一次请求不需要填写
    - limit，获取的文件数量，默认 100，最大 10000
    - order，`asc` 或 `desc`，按最后修改时间升序或降序排列，默认 `asc`
    - timeout，可选，请求超时时间，默认 1 分钟

返回值说明：

  - Result，为当前目录下各个文件/子目录的属性及对应的值的列表，属性包括 `文件名`、`类型`、`大小`、`最后修改时间`，示例：

    ```
    [[{name,<<"image.jpg">>},
      {type,file},
      {size,2080825},
      {last_modified,1486805355}],
     [{name,<<"dir">>},
      {type,directory},
      {size,0},
      {last_modified,1486805355}]]
    ```

  - Iter，返回下一次分页开始位置，它由一串 Base64 编码的随机数组成，当它是 `eof` 时，表明本次请求已经是最后一次分页

#### 获取空间使用情况

```erlang
{ok, Result} = upyun:usage(Client).
```

返回值说明：

  - Result，为服务的使用量（单位比特）

#### 更新元数据

```erlang
ok = upyun:metadata(Client, Key, Option, Metas).
```

参数说明：

  - Option，原数据更新类型，其可选的值有：
    - `merge`，合并文件元信息，相同的元信息将被新上传的值替换，默认
    - `replace`，替换文件元信息为新上传的文件元信息
    - `delete`，删除文件元信息
  - Metas，`proplists` 类型的元数据列表，如 `[{<<"Foo">>, <<"Bar">>}]`
  - UpdateLastModified，可选，`boolean` 类型，表示是否更新文件的最后修改时间，默认 `false` （不更新）

#### 缓存刷新

```erlang
{ok, Result} = upyun:purge(Client, Keys).
```

参数说明：

  - Keys，待刷新文件的 Key 列表

返回值说明：

  - Result，为 JSON 字符串格式，如 `{invalid_domain_of_url: 不属于自己域名的 url 列表}`

错误信息及响应状态参看[缓存刷新响应状态](https://docs.upyun.com/api/purge/#_2)。


## License

[MIT](./LICENSE)
