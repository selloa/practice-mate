# Deploy to GitHub Pages

## Prerequisites

1. Make sure your project is pushed to a GitHub repository
2. Ensure you have write access to the repository

## Steps to Deploy

### 1. Enable GitHub Pages

1. Go to your GitHub repository
2. Click on **Settings** tab
3. Scroll down to **Pages** section in the left sidebar
4. Under **Source**, select **Deploy from a branch**
5. Choose **gh-pages** branch and **/(root)** folder
6. Click **Save**

### 2. Push Your Code

The GitHub Actions workflow will automatically trigger when you push to the main branch:

```bash
git add .
git commit -m "Add GitHub Pages deployment workflow"
git push origin main
```

### 3. Monitor Deployment

1. Go to your repository on GitHub
2. Click on **Actions** tab
3. You should see the "Deploy to GitHub Pages" workflow running
4. Wait for it to complete (usually takes 2-3 minutes)

### 4. Access Your Site

Once deployment is complete, your site will be available at:
`https://[your-username].github.io/[repository-name]/`

## Troubleshooting

### If the workflow fails:

1. Check the **Actions** tab for error messages
2. Common issues:
   - Missing dependencies in `package.json`
   - Elm compilation errors
   - Node.js version compatibility

### If the site doesn't load:

1. Verify GitHub Pages is enabled in repository settings
2. Check that the `gh-pages` branch was created
3. Ensure the `compiled/` directory contains your built files

## Manual Deployment (if needed)

If you prefer to deploy manually:

```bash
# Build the project
npm run build

# Deploy using gh-pages package
npx gh-pages -d compiled
```

## Notes

- The workflow builds your project using `npm run build`
- The built files are deployed from the `compiled/` directory
- Each push to main/master will trigger a new deployment
- The site will be available at your GitHub Pages URL
