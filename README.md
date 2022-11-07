
# Table of Contents

1.  [Pretty Pages](#orgbc15a82)
    1.  [Example](#orgebf6640)



<a id="orgbc15a82"></a>

# Pretty Pages

Pretty-pages is a Emacs Package that helps you create your webpages in a format that makes your URL pretty.
It makes every webpage you create into the form foo/index.html so you can remove the .html in your website.


<a id="orgebf6640"></a>

## Example

<https://parallelepiped.srht.site/projects/>
You can see that there is no `.html` extention.
This is a achieved by having your webpage (let's say projects) in the form `projects/index.html` and in the link remove the entire file and leave only the folder. The browser will detect and switch to the actual webpage automatically but the link will stay the same.

Without setting up your files this way, your URLs will look like these:

1.  <https://parallelepiped.srht.site/projects.html>
2.  <https://parallelepiped.srht.site/projects/projects.html>

