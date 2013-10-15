<head>
  <link href="css/signin.css" rel="stylesheet" media="screen">
</head>
<div class="container">
  <form class="form-signin" method="post" action="${postAction}">
    <h2 class="form-signin-heading">Please sign in</h2>
    <input type="text" class="form-control" name="login" placeholder="Email address" autofocus>
    <input type="password" class="form-control" name="password" placeholder="Password">
    <label class="checkbox">
      <input type="checkbox" name="remember-me" value="remember-me"> Remember me
    </label>
    <button class="btn btn-lg btn-primary btn-block" type="submit"><submitText/></button>
  </form>
</div>
