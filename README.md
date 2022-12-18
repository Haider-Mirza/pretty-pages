
# Table of Contents

1.  [Pretty Pages](#org5b2992f)
2.  [Documentation](#org17acdf9)
    1.  [What Pretty Pages do?](#org7b72deb)



<a id="org5b2992f"></a>

# Pretty Pages

Pretty-pages is a Emacs Package that helps you create your webpages in a format that makes your URL pretty. It makes every webpage you create into the form of **foo/index.html** so you can remove the .html in your website. This package is inspired by [Hugo](https://gohugo.io/).


<a id="org17acdf9"></a>

# Documentation

You can find documentation on my website:
<https://parallelepiped.srht.site/projects/pretty-pages/>


<a id="org7b72deb"></a>

## What Pretty Pages do?

<https://parallelepiped.srht.site/projects/>
You can see that in this URL, there is no **.html** extention.
This is a achieved by having your webpage (let's say projects) in the form **projects/index.html** and in the link remove the entire file and leave only the folder. The majority of browsers will detect and switch to the actual webpage automatically but the link will stay the same.

Without setting up your files this way, your URLs will look like these:

1.  <https://parallelepiped.srht.site/projects.html>
2.  <https://parallelepiped.srht.site/projects/projects.html>

Otherwise, with Pretty Pages, your URLs would look like this: 

1.  <https://parallelepiped.srht.site/projects/>
2.  <https://parallelepiped.srht.site/projects/projects/>

